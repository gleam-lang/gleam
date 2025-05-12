use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use ecow::EcoString;
use lsp_types::{
    Documentation, MarkupContent, MarkupKind, ParameterInformation, ParameterLabel, SignatureHelp,
    SignatureInformation,
};

use crate::{
    ast::{CallArg, ImplicitCallArgOrigin, TypedExpr},
    build::Module,
    type_::{FieldMap, ModuleValueConstructor, Type, printer::Printer},
};

pub fn for_expression(expr: &TypedExpr, module: &Module) -> Option<SignatureHelp> {
    // If we're inside a function call we can provide signature help,
    // otherwise we don't want anything to pop up.
    let TypedExpr::Call { fun, arguments, .. } = expr else {
        return None;
    };

    match fun.as_ref() {
        // If the thing being called is a local variable then we want to
        // use it's name as the function name to be used in the signature
        // help.
        TypedExpr::Var {
            constructor, name, ..
        } => signature_help(
            name.clone(),
            fun,
            arguments,
            constructor.field_map(),
            module,
        ),

        // If we're making a qualified call to another module's function
        // then we want to show its type, documentation and the exact name
        // being used (that is "<module_name>.<function_name>").
        //
        //   eg. list.map(|)
        //                ^ When the cursor is here we are going to show
        //                  "list.map(List(a), with: fn(a) -> b) -> List(b)"
        //                  as the help signature.
        //
        TypedExpr::ModuleSelect {
            module_alias,
            label,
            constructor,
            ..
        } => {
            let field_map = match constructor {
                ModuleValueConstructor::Constant { .. } => None,
                ModuleValueConstructor::Record { field_map, .. }
                | ModuleValueConstructor::Fn { field_map, .. } => field_map.into(),
            };
            let name = format!("{module_alias}.{label}").into();
            signature_help(name, fun, arguments, field_map, module)
        }

        // If the function bein called is an invalid node we don't want to
        // provide any hint, otherwise one might be under the impression that
        // that function actually exists somewhere.
        //
        TypedExpr::Invalid { .. } => None,

        // In all other cases we can't figure out a good name to show in the
        // signature help so we use an anonymous `fn` as the name to be
        // shown.
        //
        //     eg. fn(a){a}(|)
        //                  ^ When the cursor is here we are going to show
        //                    "fn(a: a) -> a" as the help signature.
        //
        _ => signature_help("fn".into(), fun, arguments, None, module),
    }
}

/// Show the signature help of a function with the given name.
/// Besides the function's typed expression `fun`, this function needs a bit of
/// additional data to properly display a useful help signature:
///
/// - `fun_name` is used as the display name of the function in the help
///   signature.
/// - `supplied_arguments` are arguments being passed to the function call, those
///   might not be of the correct arity or have wrong types but are used to
///   deduce which argument should be highlighted next in the help signature.
/// - `field_map` is the function's field map (if any) that will be used to
///   display labels and understand which labelled argument should be
///   highlighted next in the help signature.
///
fn signature_help(
    fun_name: EcoString,
    fun: &TypedExpr,
    supplied_arguments: &[CallArg<TypedExpr>],
    field_map: Option<&FieldMap>,
    module: &Module,
) -> Option<SignatureHelp> {
    let (arguments, return_) = fun.type_().fn_types()?;

    // If the function has no arguments, we don't want to show any help.
    let arity = arguments.len() as u32;
    if arity == 0 {
        return None;
    }

    let index_to_label = match field_map {
        Some(field_map) => field_map
            .fields
            .iter()
            .map(|(name, index)| (*index, name))
            .collect(),
        None => HashMap::new(),
    };

    let printer = Printer::new(&module.ast.names);
    let (label, parameters) =
        print_signature_help(printer, fun_name, arguments, return_, &index_to_label);

    let active_parameter = active_parameter_index(arity, supplied_arguments, index_to_label)
        // If we don't want to highlight any arg in the suggestion we have to
        // explicitly provide an out of bound index.
        .or(Some(arity));

    Some(SignatureHelp {
        signatures: vec![SignatureInformation {
            label,
            documentation: fun.get_documentation().map(|d| {
                Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: d.into(),
                })
            }),
            parameters: Some(parameters),
            active_parameter: None,
        }],
        active_signature: Some(0),
        active_parameter,
    })
}

fn active_parameter_index(
    arity: u32,
    supplied_arguments: &[CallArg<TypedExpr>],
    mut index_to_label: HashMap<u32, &EcoString>,
) -> Option<u32> {
    let mut is_use_call = false;
    let mut found_labelled_argument = false;
    let mut used_labels = HashSet::new();

    let mut supplied_unlabelled_arguments = 0;
    let unlabelled_arguments = arity - index_to_label.len() as u32;

    for (i, arg) in supplied_arguments.iter().enumerate() {
        // If there's an unlabelled argument after a labelled one, we can't
        // figure out what to suggest since arguments were passed in a wrong
        // order.
        if found_labelled_argument && arg.label.is_none() && !arg.is_implicit() {
            return None;
        }

        // Once we reach to an implicit use argument (be it the callback or the
        // missing implicitly inserted ones) we can break since those must be
        // the last arguments of the function and are not explicitly supplied by
        // the programmer.
        if let Some(ImplicitCallArgOrigin::Use | ImplicitCallArgOrigin::IncorrectArityUse) =
            arg.implicit
        {
            is_use_call = true;
            break;
        }

        match &arg.label {
            Some(label) => {
                found_labelled_argument = true;
                let _ = used_labels.insert(label);
            }

            // If the argument is unlabelled we just remove the label
            // corresponding to it from the field map since it has already been
            // passed as an unlabelled argument.
            None => {
                supplied_unlabelled_arguments += 1;
                let _ = index_to_label.remove(&(i as u32));
            }
        }
    }

    let active_index = if supplied_unlabelled_arguments < unlabelled_arguments {
        if found_labelled_argument {
            // If I have supplied some labelled args but I haven't supplied all
            // unlabelled args before a labelled one then we can't safely
            // suggest anything as the next argument.
            None
        } else {
            // If I haven't supplied enough unlabelled arguments then I have to
            // set the next one as active (be it labelled or not).
            Some(supplied_unlabelled_arguments)
        }
    } else {
        // If I have supplied all the unlabelled arguments (and we could have
        // also supplied some labelled ones as unlabelled!) then we pick the
        // leftmost labelled argument that hasn't been supplied yet.
        index_to_label
            .into_iter()
            .filter(|(_index, label)| !used_labels.contains(label))
            .map(|(index, _label)| index)
            .min()
            .or(Some(supplied_arguments.len() as u32))
    };

    // If we're showing hints for a use call and we end up deciding that the
    // only index we can suggest is the one of the use callback then we do not
    // highlight it or it would lead people into believing they can manually
    // pass that argument in.
    if is_use_call && active_index == Some(arity - 1) {
        None
    } else {
        active_index
    }
}

/// To produce a signature that can be used by the LS, we need to also keep
/// track of the arguments' positions in the printed signature. So this function
/// prints the signature help producing at the same time a list of correct
/// `ParameterInformation` for all its arguments.
///
fn print_signature_help(
    mut printer: Printer<'_>,
    function_name: EcoString,
    arguments: Vec<Arc<Type>>,
    return_: Arc<Type>,
    index_to_label: &HashMap<u32, &EcoString>,
) -> (String, Vec<ParameterInformation>) {
    let arguments_count = arguments.len();
    let mut signature = format!("{function_name}(");
    let mut parameter_informations = Vec::with_capacity(arguments_count);

    for (i, argument) in arguments.iter().enumerate() {
        let arg_start = signature.len();
        if let Some(label) = index_to_label.get(&(i as u32)) {
            signature.push_str(label);
            signature.push_str(": ");
        }
        signature.push_str(&printer.print_type(argument));
        let arg_end = signature.len();
        let label = ParameterLabel::LabelOffsets([arg_start as u32, arg_end as u32]);

        parameter_informations.push(ParameterInformation {
            label,
            documentation: None,
        });

        let is_last = i == arguments_count - 1;
        if !is_last {
            signature.push_str(", ");
        }
    }

    signature.push_str(") -> ");
    signature.push_str(&printer.print_type(&return_));
    (signature, parameter_informations)
}
