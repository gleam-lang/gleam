use std::path::PathBuf;

use gleam_core::{
    build::{Codegen, Mode, Options, Runtime, Target},
    config::{DenoFlag, PackageConfig},
    error::Error,
    io::{CommandExecutor, Stdio},
    paths::ProjectPaths,
};
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

    // Validate the module path
    if let Some(mod_path) = &module {
        if !crate::module::is_gleam_module(mod_path) {
            return Err(Error::InvalidModuleName {
                module: mod_path.to_owned(),
            });
        }
    };

    // Download dependencies
    let manifest = crate::build::download_dependencies()?;

    // Get the config for the module that is being run to check the target.
    let mod_config = match &module {
        Some(mod_path) => crate::config::module_config(mod_path, &paths),
        _ => crate::config::root_config(),
    }?;
    // The root config is required to run the project.
    let root_config = crate::config::root_config()?;

    // Determine which module to run
    let module = module.unwrap_or(match which {
        Which::Src => root_config.name.to_string(),
        Which::Test => format!("{}_test", &root_config.name),
    });

    let target = target.unwrap_or(mod_config.target);

    // Build project so we have bytecode to run
    let built = crate::build::main(
        Options {
            warnings_as_errors: false,
            codegen: Codegen::All,
            mode: Mode::Dev,
            target: Some(target),
        },
        Some(manifest),
    )?;

    // A module can not be run if it does not exist or does not have a public main function.
    let main_function = built.get_main_function(&SmolStr::from(module.to_owned()))?;

    // Don't exit on ctrl+c as it is used by child erlang shell
    ctrlc::set_handler(move || {}).expect("Error setting Ctrl-C handler");

    crate::cli::print_running(&format!("{module}.main"));

    // Run the command
    let status = match target {
        Target::Erlang => match runtime {
            Some(r) => Err(Error::InvalidRuntime {
                target: Target::Erlang,
                invalid_runtime: r,
            }),
            _ => run_erlang(&paths, &root_config.name, &module, arguments),
        },
        Target::JavaScript => match runtime.unwrap_or(root_config.javascript.runtime) {
            Runtime::Deno => run_javascript_deno(
                &paths,
                &root_config,
                &main_function.package,
                &module,
                arguments,
            ),
            Runtime::NodeJs => {
                run_javascript_node(&paths, &main_function.package, &module, arguments)
            }
        },
    }?;

    std::process::exit(status);
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
    let entry = write_javascript_entrypoint(paths, package, module)?;

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

    let entrypoint = write_javascript_entrypoint(paths, package, module)?;
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
