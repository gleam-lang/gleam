use std::sync::OnceLock;

use camino::Utf8PathBuf;
use ecow::EcoString;
use gleam_core::{
    analyse::TargetSupport,
    build::{Built, Codegen, Compile, Mode, NullTelemetry, Options, Runtime, Target, Telemetry},
    config::{DenoFlag, PackageConfig},
    error::Error,
    io::{Command, CommandExecutor, Stdio},
    paths::ProjectPaths,
    type_::ModuleFunction,
};

use crate::{config::PackageKind, fs::ProjectIO};

#[derive(Debug, Clone, Copy)]
pub enum Which {
    Src,
    Test,
    Dev,
}

// TODO: test
pub fn command(
    paths: &ProjectPaths,
    arguments: Vec<String>,
    target: Option<Target>,
    runtime: Option<Runtime>,
    module: Option<String>,
    which: Which,
    no_print_progress: bool,
) -> Result<(), Error> {
    // Don't exit on ctrl+c as it is used by child erlang shell
    ctrlc::set_handler(move || {}).expect("Error setting Ctrl-C handler");
    let command = setup(
        paths,
        arguments,
        target,
        runtime,
        module,
        which,
        no_print_progress,
    )?;
    let status = ProjectIO::new().exec(command)?;
    std::process::exit(status);
}

pub fn setup(
    paths: &ProjectPaths,
    arguments: Vec<String>,
    target: Option<Target>,
    runtime: Option<Runtime>,
    module_arg: Option<String>,
    which: Which,
    no_print_progress: bool,
) -> Result<Command, Error> {
    // Validate the module path
    if let Some(mod_path) = &module_arg {
        if !is_gleam_module(mod_path) {
            return Err(Error::InvalidModuleName {
                module: mod_path.to_owned(),
            });
        }
    };

    let telemetry: &'static dyn Telemetry = if no_print_progress {
        &NullTelemetry
    } else {
        &crate::cli::Reporter
    };

    // Download dependencies
    let manifest = if no_print_progress {
        crate::build::download_dependencies(paths, NullTelemetry)?
    } else {
        crate::build::download_dependencies(paths, crate::cli::Reporter::new())?
    };

    // Get the config for the module that is being run to check the target.
    // Also get the kind of the package the module belongs to: wether the module
    // belongs to a dependency or to the root package.
    let (mod_config, package_kind) = match &module_arg {
        Some(mod_path) => {
            crate::config::find_package_config_for_module(mod_path, &manifest, paths)?
        }
        _ => (crate::config::root_config(paths)?, PackageKind::Root),
    };

    // The root config is required to run the project.
    let root_config = crate::config::root_config(paths)?;

    // Determine which module to run
    let module = module_arg.clone().unwrap_or(match which {
        Which::Src => root_config.name.to_string(),
        Which::Test => format!("{}_test", &root_config.name),
        Which::Dev => format!("{}_dev", &root_config.name),
    });

    let target = target.unwrap_or(mod_config.target);

    let options = Options {
        warnings_as_errors: false,
        compile: match package_kind {
            // If we're trying to run a dependecy module we do not compile and
            // check the root package. So we can run the main function from a
            // dependency's module even if the root package doesn't compile.
            PackageKind::Dependency => Compile::DepsOnly,
            PackageKind::Root => Compile::All,
        },
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
        no_print_progress,
    };

    let built = crate::build::main(paths, options, manifest)?;

    // A module can not be run if it does not exist or does not have a public main function.
    let main_function = get_or_suggest_main_function(built, &module, target)?;

    telemetry.running(&format!("{module}.main"));

    // Get the command to run the project.
    match target {
        Target::Erlang => match runtime {
            Some(r) => Err(Error::InvalidRuntime {
                target: Target::Erlang,
                invalid_runtime: r,
            }),
            _ => run_erlang_command(
                paths,
                &root_config.name,
                &module,
                arguments,
                module_arg.is_none(),
            ),
        },
        Target::JavaScript => match runtime.unwrap_or(mod_config.javascript.runtime) {
            Runtime::Deno => run_javascript_deno_command(
                paths,
                &root_config,
                &main_function.package,
                &module,
                arguments,
            ),
            Runtime::NodeJs => {
                run_javascript_node_command(paths, &main_function.package, &module, arguments)
            }
            Runtime::Bun => {
                run_javascript_bun_command(paths, &main_function.package, &module, arguments)
            }
        },
    }
}

fn run_erlang_command(
    paths: &ProjectPaths,
    package: &str,
    module: &str,
    arguments: Vec<String>,
    use_entrypoint: bool,
) -> Result<Command, Error> {
    let mut args = vec![];

    // Specify locations of Erlang applications
    let packages = paths.build_directory_for_target(Mode::Dev, Target::Erlang);

    for entry in crate::fs::read_dir(packages)?.filter_map(Result::ok) {
        args.push("-pa".into());
        args.push(entry.path().join("ebin").into());
    }

    // gleam modules are separated by `/`. Erlang modules are separated by `@`.
    let module = module.replace('/', "@");
    // Decide what to evaluate
    let erl_eval = if use_entrypoint {
        format!("{package}@@main:run({module})")
    } else {
        format!("{module}:main(), init:stop()")
    };
    args.push("-eval".into());
    args.push(erl_eval);

    // Don't run the Erlang shell
    args.push("-noshell".into());

    // Tell the BEAM that any following argument are for the program
    args.push("-extra".into());
    for argument in arguments.into_iter() {
        args.push(argument);
    }

    Ok(Command {
        program: "erl".to_string(),
        args,
        env: vec![],
        cwd: None,
        stdio: Stdio::Inherit,
    })
}

fn run_javascript_bun_command(
    paths: &ProjectPaths,
    package: &str,
    module: &str,
    arguments: Vec<String>,
) -> Result<Command, Error> {
    let mut args = vec!["run".to_string()];
    let entry = write_javascript_entrypoint(paths, package, module)?;

    args.push(entry.to_string());

    for arg in arguments.into_iter() {
        args.push(arg);
    }

    Ok(Command {
        program: "bun".to_string(),
        args,
        env: vec![],
        cwd: None,
        stdio: Stdio::Inherit,
    })
}

fn run_javascript_node_command(
    paths: &ProjectPaths,
    package: &str,
    module: &str,
    arguments: Vec<String>,
) -> Result<Command, Error> {
    let mut args = vec![];
    let entry = write_javascript_entrypoint(paths, package, module)?;

    args.push(entry.to_string());

    for argument in arguments.into_iter() {
        args.push(argument);
    }

    Ok(Command {
        program: "node".to_string(),
        args,
        env: vec![],
        cwd: None,
        stdio: Stdio::Inherit,
    })
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

fn run_javascript_deno_command(
    paths: &ProjectPaths,
    config: &PackageConfig,
    package: &str,
    module: &str,
    arguments: Vec<String>,
) -> Result<Command, Error> {
    let mut args = vec![];

    // Run the main function.
    args.push("run".into());

    // Enable unstable features and APIs
    if config.javascript.deno.unstable {
        args.push("--unstable".into())
    }

    // Enable location API
    if let Some(location) = &config.javascript.deno.location {
        args.push(format!("--location={location}"));
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

    Ok(Command {
        program: "deno".to_string(),
        args,
        env: vec![],
        cwd: None,
        stdio: Stdio::Inherit,
    })
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

    // Otherwise see if the module has been prefixed with "src/", "test/" or "dev/".
    for prefix in ["src/", "test/", "dev/"] {
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
///Build a throw-away directory under the system TMP dir.
fn fresh_tmp_dir() -> Utf8PathBuf {
    use std::{env, fs, time::*};
    let mut dir = env::temp_dir();
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    dir.push(format!("gleam_run_test_{nanos:x}"));
    fs::create_dir_all(&dir).unwrap();
    Utf8PathBuf::from_path_buf(dir).expect("temp dir is valid UTF-8")
}

/// Create the minimal directory structure that `run_erlang_command` walks
fn prepare_erlang_build_tree(root: &std::path::Path) {
    use std::fs;
    let ebin = root
        .join("build")
        .join("dev")
        .join("erlang")
        .join("dummy_pkg")
        .join("ebin");
    fs::create_dir_all(ebin).unwrap();
}

#[test]
fn run_erlang_command_uses_entrypoint_when_requested() {
    use gleam_core::paths::ProjectPaths;
    let root = fresh_tmp_dir();
    prepare_erlang_build_tree(root.as_std_path());
    let paths = ProjectPaths::new(root);
    //call helper with entry point
    let cmd = run_erlang_command(&paths, "myApp", "foo/bar", Vec::new(), true)
        .expect("command build failed");

    let eval_arg = cmd
        .args
        .iter()
        .skip_while(|s| *s != "-eval")
        .nth(1)
        .expect("no -eval arg produced");
    assert!(
        eval_arg.contains("@@main:run("),
        "expected entry-point call, got {eval_arg}"
    );
}

#[test]
fn run_erlang_command_skips_entrypoint_when_module_flag_used() {
    use gleam_core::paths::ProjectPaths;
    let root = fresh_tmp_dir();
    prepare_erlang_build_tree(root.as_std_path());
    let paths = ProjectPaths::new(root);
    //call helper without entry point
    let cmd = run_erlang_command(&paths, "myApp", "foo/bar", Vec::new(), false)
        .expect("command build failed");
    let eval_arg = cmd
        .args
        .iter()
        .skip_while(|s| *s != "-eval")
        .nth(1)
        .expect("no -eval arg produced");
    assert!(
        eval_arg.ends_with(":main(), init:stop()"),
        "expected direct main/0 call followed by init:stop(), got {eval_arg}"
    );
}
