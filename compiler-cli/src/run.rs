use std::path::PathBuf;

use gleam_core::{
    build::{Codegen, Mode, Options, Runtime, Target},
    config::{DenoFlag, PackageConfig},
    error::Error,
    io::{CommandExecutor, Stdio},
    paths::ProjectPaths,
};
use lazy_static::lazy_static;
use smol_str::SmolStr;

use crate::fs::ProjectIO;

#[derive(Debug, Clone, Copy)]
pub enum Which {
    Src,
    Test,
}

// TODO: test
pub fn command(
    arguments: Vec<String>,
    target: Option<Target>,
    runtime: Option<Runtime>,
    module: Option<String>,
    which: Which,
) -> Result<(), Error> {
    let paths = crate::project_paths_at_current_directory();
    // Validate the module to make sure it is a gleam module path
    match &module {
        Some(module_name) => {
            // TODO: Check if this can be replaced with a function that
            // someone already wrote
            if !is_gleam_module(&module_name) {
                Err(Error::InvalidModuleName {
                    module: module_name.to_owned(),
                })
            } else {
                Ok(())
            }
        }
        None => Ok(()),
    }?;

    let config = crate::config::root_config()?;

    // Determine which module to run
    let module = module.unwrap_or(match which {
        Which::Src => config.name.to_string(),
        Which::Test => format!("{}_test", &config.name),
    });

    // Build project so we have bytecode to run
    let built = crate::build::main(Options {
        warnings_as_errors: false,
        codegen: Codegen::All,
        mode: Mode::Dev,
        target,
    })?;

    // A module can not be run if it does not exist or does not have a public main function.
    let main_function = match built
        .module_interfaces
        .get(&SmolStr::from(module.to_owned()))
    {
        Some(module_data) => match module_data.get_function(&SmolStr::from("main")) {
            Some(function) => {
                if function.arity == 0 {
                    Ok(function)
                } else {
                    Err(Error::MainFunctionHasWrongArity {
                        module: module.to_owned(),
                        arity: function.arity,
                    })
                }
            }
            None => Err(Error::ModuleDoesNotHaveMainFunction {
                module: module.to_owned(),
            }),
        },
        None => Err(Error::ModuleDoesNotExist {
            module: module.to_owned(),
        }),
    }?;

    // Don't exit on ctrl+c as it is used by child erlang shell
    ctrlc::set_handler(move || {}).expect("Error setting Ctrl-C handler");

    crate::cli::print_running(&format!("{module}.main"));

    // Run the command
    let status = match target.unwrap_or(config.target) {
        Target::Erlang => match runtime {
            Some(r) => Err(Error::InvalidRuntime {
                target: Target::Erlang,
                invalid_runtime: r,
            }),
            _ => run_erlang(&paths, &config.name, &module, arguments),
        },
        Target::JavaScript => match runtime.unwrap_or(config.javascript.runtime) {
            Runtime::Deno => {
                run_javascript_deno(&paths, &config, &main_function.package, &module, arguments)
            }
            Runtime::NodeJs => {
                run_javascript_node(&paths, &main_function.package, &module, arguments)
            }
        },
    }?;

    std::process::exit(status);
}

fn is_gleam_module(module: &str) -> bool {
    use regex::Regex;
    lazy_static! {
        static ref RE: Regex = Regex::new(&format!(
            "^({module}{slash})*{module}$",
            module = "[a-z][_a-z0-9]*",
            slash = "/",
        ))
        .expect("is_gleam_module() RE regex");
    }

    RE.is_match(module)
}

fn run_erlang(
    paths: &ProjectPaths,
    package: &str,
    module: &str,
    arguments: Vec<String>,
) -> Result<i32, Error> {
    let mut args = vec![];

    // Specify locations of Erlang applications
    let packages = paths.build_directory_for_target(Mode::Dev, Target::Erlang);

    for entry in crate::fs::read_dir(packages)?.filter_map(Result::ok) {
        args.push("-pa".into());
        args.push(entry.path().join("ebin").to_string_lossy().into());
    }

    // gleam modules are seperated by `/`. Erlang modules are seperated by `@`.
    let module = module.replace('/', "@");

    args.push("-eval".into());
    args.push(format!("{package}@@main:run({module})"));

    // Don't run the Erlang shell
    args.push("-noshell".into());

    // Tell the BEAM that any following argument are for the program
    args.push("-extra".into());
    for argument in arguments.into_iter() {
        args.push(argument);
    }

    ProjectIO::new().exec("erl", &args, &[], None, Stdio::Inherit)
}

fn run_javascript_node(
    paths: &ProjectPaths,
    package: &str,
    module: &str,
    arguments: Vec<String>,
) -> Result<i32, Error> {
    let mut args = vec![];
    let entry = write_javascript_entrypoint(paths, &package, module)?;

    args.push(entry);

    for argument in arguments.into_iter() {
        args.push(argument);
    }

    ProjectIO::new().exec("node", &args, &[], None, Stdio::Inherit)
}

fn write_javascript_entrypoint(
    paths: &ProjectPaths,
    package: &str,
    module: &str,
) -> Result<String, Error> {
    let entry = paths
        .build_directory_for_package(Mode::Dev, Target::JavaScript, package)
        .strip_prefix(paths.root())
        .expect("Failed to strip prefix from path")
        .to_path_buf();
    let entrypoint = format!("./{}/gleam.main.mjs", entry.to_string_lossy());
    let module = format!(
        r#"import {{ main }} from "./{module}.mjs";
main();
"#,
    );
    crate::fs::write(&PathBuf::from(&entrypoint), &module)?;
    Ok(entrypoint)
}

fn run_javascript_deno(
    paths: &ProjectPaths,
    config: &PackageConfig,
    package: &str,
    module: &str,
    arguments: Vec<String>,
) -> Result<i32, Error> {
    let mut args = vec![];

    // Run the main function.
    args.push("run".into());

    // Set deno permissions
    if config.javascript.deno.allow_all {
        // Allow all
        args.push("--allow-all".into())
    } else {
        // Allow env
        add_deno_flag(&mut args, "--allow-env", &config.javascript.deno.allow_env);

        // Allow sys
        if config.javascript.deno.allow_sys {
            args.push("--allow-sys".into())
        }

        // Allow hrtime
        if config.javascript.deno.allow_hrtime {
            args.push("--allow-hrtime".into())
        }

        // Allow net
        add_deno_flag(&mut args, "--allow-net", &config.javascript.deno.allow_net);

        // Allow ffi
        if config.javascript.deno.allow_ffi {
            args.push("--allow-ffi".into())
        }

        // Allow read
        add_deno_flag(
            &mut args,
            "--allow-read",
            &config.javascript.deno.allow_read,
        );

        // Allow run
        add_deno_flag(&mut args, "--allow-run", &config.javascript.deno.allow_run);

        // Allow write
        add_deno_flag(
            &mut args,
            "--allow-write",
            &config.javascript.deno.allow_write,
        );
    }

    let entrypoint = write_javascript_entrypoint(paths, &package, module)?;
    args.push(entrypoint);

    for argument in arguments.into_iter() {
        args.push(argument);
    }

    ProjectIO::new().exec("deno", &args, &[], None, Stdio::Inherit)
}

fn add_deno_flag(args: &mut Vec<String>, flag: &str, flags: &DenoFlag) {
    match flags {
        DenoFlag::AllowAll => args.push(flag.to_owned()),
        DenoFlag::Allow(allow) => {
            if !allow.is_empty() {
                args.push(format!("{}={}", flag.to_owned(), allow.join(",")));
            }
        }
    }
}

#[test]
fn invalid_module_names() {
    for mod_name in [
        "",
        "/mod/name",
        "/mod/name/",
        "mod/name/",
        "/mod/",
        "mod/",
        "common-invalid-character",
    ] {
        assert!(!is_gleam_module(mod_name));
    }
}

#[test]
fn valid_module_names() {
    for mod_name in ["valid", "valid/name", "valid/mod/name"] {
        assert!(is_gleam_module(mod_name));
    }
}
