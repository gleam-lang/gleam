use std::{sync::Arc, sync::OnceLock};

use camino::Utf8PathBuf;
use ecow::EcoString;
use gleam_core::{
    analyse::TargetSupport,
    build::{Built, Codegen, Mode, NullTelemetry, Options, Runtime, Target, Telemetry},
    config::{DenoFlag, PackageConfig},
    error::Error,
    io::{CommandExecutor, Stdio},
    paths::ProjectPaths,
    type_::ModuleFunction,
};

use crate::{config::PackageKind, fs::ProjectIO};

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
    no_print_progress: bool,
    which: Which,
) -> Result<(), Error> {
    let paths = crate::find_project_paths()?;

    // Validate the module path
    if let Some(mod_path) = &module {
        if !is_gleam_module(mod_path) {
            return Err(Error::InvalidModuleName {
                module: mod_path.to_owned(),
            });
        }
    };

    // Download dependencies
    let manifest = crate::build::download_dependencies()?;

    // Get the config for the module that is being run to check the target.
    // Also get the kind of the package the module belongs to: wether the module
    // belongs to a dependency or to the root package.
    let (mod_config, package_kind) = match &module {
        Some(mod_path) => {
            crate::config::find_package_config_for_module(mod_path, &manifest, &paths)?
        }
        _ => (crate::config::root_config()?, PackageKind::Root),
    };

    // The root config is required to run the project.
    let root_config = crate::config::root_config()?;

    // Determine which module to run
    let module = module.unwrap_or(match which {
        Which::Src => root_config.name.to_string(),
        Which::Test => format!("{}_test", &root_config.name),
    });

    let target = target.unwrap_or(mod_config.target);

    let options = Options {
        warnings_as_errors: false,
        codegen: Codegen::All,
        mode: Mode::Dev,
        target: Some(target),
        root_target_support: match package_kind {
            // The module we want to run is in the root package, so we make sure that the package
            // can compile successfully for the current target.
            PackageKind::Root => TargetSupport::Enforced,
            // On the other hand, if we're trying to run a module that belongs to a dependency, we
            // only care if the dependency can compile for the current target.
            PackageKind::Dependency => TargetSupport::NotEnforced,
        },
    };

    let telemetry: Arc<dyn Telemetry> = if no_print_progress {
        Arc::new(NullTelemetry)
    } else {
        Arc::new(crate::cli::Reporter::new())
    };

    let built = crate::build::main(options, manifest, telemetry.clone())?;

    // A module can not be run if it does not exist or does not have a public main function.
    let main_function = get_or_suggest_main_function(built, &module, target)?;

    // Don't exit on ctrl+c as it is used by child erlang shell
    ctrlc::set_handler(move || {}).expect("Error setting Ctrl-C handler");

    telemetry.running_module(&module);

    // Run the command
    let status = match target {
        Target::Erlang => match runtime {
            Some(r) => Err(Error::InvalidRuntime {
                target: Target::Erlang,
                invalid_runtime: r,
            }),
            _ => run_erlang(&paths, &root_config.name, &module, arguments),
        },
        Target::JavaScript => match runtime.unwrap_or(mod_config.javascript.runtime) {
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
            Runtime::Bun => run_javascript_bun(&paths, &main_function.package, &module, arguments),
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
        args.push(entry.path().join("ebin").into());
    }

    // gleam modules are separated by `/`. Erlang modules are separated by `@`.
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

fn run_javascript_bun(
    paths: &ProjectPaths,
    package: &str,
    module: &str,
    arguments: Vec<String>,
) -> Result<i32, Error> {
    let mut args = vec!["run".to_string()];
    let entry = write_javascript_entrypoint(paths, package, module)?;

    args.push(entry.to_string());

    for arg in arguments.into_iter() {
        args.push(arg);
    }

    ProjectIO::new().exec("bun", &args, &[], None, Stdio::Inherit)
}

fn run_javascript_node(
    paths: &ProjectPaths,
    package: &str,
    module: &str,
    arguments: Vec<String>,
) -> Result<i32, Error> {
    let mut args = vec![];
    let entry = write_javascript_entrypoint(paths, package, module)?;

    args.push(entry.to_string());

    for argument in arguments.into_iter() {
        args.push(argument);
    }

    ProjectIO::new().exec("node", &args, &[], None, Stdio::Inherit)
}

fn write_javascript_entrypoint(
    paths: &ProjectPaths,
    package: &str,
    module: &str,
) -> Result<Utf8PathBuf, Error> {
    let path = paths
        .build_directory_for_package(Mode::Dev, Target::JavaScript, package)
        .to_path_buf()
        .join("gleam.main.mjs");
    let module = format!(
        r#"import {{ main }} from "./{module}.mjs";
main();
"#,
    );
    crate::fs::write(&path, &module)?;
    Ok(path)
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

    // Enable unstable features and APIs
    if config.javascript.deno.unstable {
        args.push("--unstable".into())
    }

    // Enable location API
    if let Some(location) = &config.javascript.deno.location {
        args.push(format!("--location={}", location));
    }

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
    args.push(entrypoint.to_string());

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

/// Check if a module name is a valid gleam module name.
fn is_gleam_module(module: &str) -> bool {
    use regex::Regex;
    static RE: OnceLock<Regex> = OnceLock::new();

    RE.get_or_init(|| {
        Regex::new(&format!(
            "^({module}{slash})*{module}$",
            module = "[a-z][_a-z0-9]*",
            slash = "/",
        ))
        .expect("is_gleam_module() RE regex")
    })
    .is_match(module)
}

/// If provided module is not executable, suggest a possible valid module.
fn get_or_suggest_main_function(
    built: Built,
    module: &str,
    target: Target,
) -> Result<ModuleFunction, Error> {
    // Check if the module exists
    let error = match built.get_main_function(&module.into(), target) {
        Ok(main_fn) => return Ok(main_fn),
        Err(error) => error,
    };

    // Otherwise see if the module has been prefixed with "src/" or "test/".
    for prefix in ["src/", "test/"] {
        let other = match module.strip_prefix(prefix) {
            Some(other) => other.into(),
            None => continue,
        };
        if built.get_main_function(&other, target).is_ok() {
            return Err(Error::ModuleDoesNotExist {
                module: EcoString::from(module),
                suggestion: Some(other),
            });
        }
    }

    Err(error)
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
