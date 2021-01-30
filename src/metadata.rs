//! Seriaisation and deserialisation of Gleam compiler metadata into binary files
//! using the Cap'n Proto schema.

// TODO: remove
#![allow(unused)]

mod module_builder;

pub use self::module_builder::ModuleBuilder;

use crate::{
    schema_capnp as schema,
    typ::{AccessorsMap, Module, Type, TypeConstructor, ValueConstructor},
    Result,
};
use std::{collections::HashMap, io::BufRead, sync::Arc};

#[derive(Debug, Default)]
pub struct ModuleDecoder {
    next_type_var_id: u64,
    type_var_id_map: HashMap<usize, u64>,
}

impl ModuleDecoder {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn read_module(&mut self, reader: impl BufRead) -> Result<Module> {
        let message_reader =
            capnp::serialize_packed::read_message(reader, capnp::message::ReaderOptions::new())?;
        let module = message_reader.get_root::<schema::module::Reader<'_>>()?;

        Ok(Module {
            name: name(&module)?,
            types: self.module_types(&module)?,
            values: self.module_values(&module)?,
            accessors: self.module_accessors(&module)?,
        })
    }

    fn module_types(
        &mut self,
        reader: &schema::module::Reader<'_>,
    ) -> Result<HashMap<String, TypeConstructor>> {
        let types_reader = reader.get_types()?;
        let mut types = HashMap::with_capacity(types_reader.len() as usize);
        for prop in types_reader.into_iter() {
            let name = prop.get_key()?;
            let type_ = self.type_constructor(&prop.get_value()?)?;
            let _ = types.insert(name.to_string(), type_);
        }
        Ok(types)
    }

    fn type_constructor(
        &mut self,
        reader: &schema::type_constructor::Reader<'_>,
    ) -> Result<TypeConstructor> {
        let type_ = self.type_(&reader.get_type()?)?;
        let reader = reader.get_parameters()?;
        let mut parameters = Vec::with_capacity(reader.len() as usize);
        for reader in reader.into_iter() {
            parameters.push(self.type_(&reader)?);
        }
        Ok(TypeConstructor {
            public: true,
            origin: Default::default(),
            module: todo!("The module isn't stored in the proto yet"),
            parameters,
            typ: type_,
        })
    }

    fn type_(&mut self, reader: &schema::type_::Reader<'_>) -> Result<Arc<Type>> {
        todo!()
    }

    fn module_values(
        &self,
        reader: &schema::module::Reader<'_>,
    ) -> Result<HashMap<String, ValueConstructor>> {
        todo!()
    }

    fn module_accessors(
        &self,
        reader: &schema::module::Reader<'_>,
    ) -> Result<HashMap<String, AccessorsMap>> {
        todo!()
    }
}

//     pub fn print_address_book() -> ::capnp::Result<()> {
//         let stdin = ::std::io::stdin();
//         let message_reader = serialize_packed::read_message(&mut stdin.lock(),
//                                                             ::capnp::message::ReaderOptions::new())?;
//         let address_book = message_reader.get_root::<address_book::Reader>()?;

//         for person in address_book.get_people()?.iter() {
//             println!("{}: {}", person.get_name()?, person.get_email()?);
//             for phone in person.get_phones()?.iter() {
//                 let type_name = match phone.get_type() {
//                     Ok(person::phone_number::Type::Mobile) => "mobile",
//                     Ok(person::phone_number::Type::Home) => "home",
//                     Ok(person::phone_number::Type::Work) => "work",
//                     Err(::capnp::NotInSchema(_)) => "UNKNOWN",
//                 };
//                 println!("  {} phone: {}", type_name, phone.get_number()?);
//             }
//             match person.get_employment().which() {
//                 Ok(person::employment::Unemployed(())) => {
//                     println!("  unemployed");
//                 }
//                 Ok(person::employment::Employer(employer)) => {
//                     println!("  employer: {}", employer?);
//                 }
//                 Ok(person::employment::School(school)) => {
//                     println!("  student at: {}", school?);
//                 }
//                 Ok(person::employment::SelfEmployed(())) => {
//                     println!("  self-employed");
//                 }
//                 Err(::capnp::NotInSchema(_)) => { }
//             }
//         }
//         Ok(())
//     }
// }

fn name(module: &schema::module::Reader<'_>) -> Result<Vec<String>> {
    let name = module
        .get_name()?
        .iter()
        .map(|s| s.map(String::from))
        .collect::<Result<_, _>>()?;
    Ok(name)
}
