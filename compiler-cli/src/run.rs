use gleam_core::{
    build::{Mode, Options, Target},
    config::PackageConfig,
    error::Error,
    io::{CommandExecutor, Stdio},
    paths,
};

use crate::fs::ProjectIO;

#[derive(Debug, Clone, Copy)]
pub enum Which {
    Src,
    Test,
}

pub fn command(arguments: Vec<String>, target: Option<Target>, which: Which) -> Result<(), Error> {
    let config = crate::config::root_config()?;

    // Determine which module to run
    let module = match which {
        Which::Src => config.name.to_string(),
        Which::Test => format!("{}_test", &config.name),
    };

    // Build project so we have bytecode to run
    let _ = crate::build::main(Options {
        perform_codegen: true,
        mode: Mode::Dev,
        target,
    })?;

    // Don't exit on ctrl+c as it is used by child erlang shell
    ctrlc::set_handler(move || {}).expect("Error setting Ctrl-C handler");

    crate::cli::print_running(&format!("{}.main", module));

    // Run the command
    let status = match target.unwrap_or(config.target) {
        Target::Erlang => run_erlang(&config.name, &module, arguments),
        Target::JavaScript => {
            let runtime: &String = &config
                .javascript
                .runtime
                .clone()
                .unwrap_or_else(|| "node".into());

            match runtime.as_str() {
                "deno" => run_javascript_deno(&config, arguments),
                "node" => run_javascript_node(&config, arguments),
                runtime => Err(Error::InvalidRuntime {
                    target: "JavaScript".into(),
                    invalid_runtime: runtime.to_owned(),
                    valid_runtimes: vec!["node".into(), "deno".into()],
                }),
            }
        }
    }?;

    std::process::exit(status);
}

fn run_erlang(package: &str, module: &str, arguments: Vec<String>) -> Result<i32, Error> {
    let mut args = vec![];

    // Specify locations of .beam files
    let packages = paths::build_packages(Mode::Dev, Target::Erlang);

    for entry in crate::fs::read_dir(packages)?.filter_map(Result::ok) {
        args.push("-pa".into());
        args.push(entry.path().join("ebin").to_string_lossy().into());
    }

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

fn run_javascript_node(config: &PackageConfig, arguments: Vec<String>) -> Result<i32, Error> {
    let mut args = vec![];

    let entry = paths::build_package(Mode::Dev, Target::JavaScript, &config.name);

    args.push(format!("./{}/gleam.main.mjs", entry.to_string_lossy()));

    // Tell Node that any following argument are for the program
    args.push("--".into());
    for argument in arguments.into_iter() {
        args.push(argument);
    }

    ProjectIO::new().exec("node", &args, &[], None, Stdio::Inherit)
}

fn run_javascript_deno(config: &PackageConfig, arguments: Vec<String>) -> Result<i32, Error> {
    let mut args = vec![];

    let entry = paths::build_package(Mode::Dev, Target::JavaScript, &config.name);

    // Run the main function.
    args.push("run".into());

    // Set deno permissions
    if config.deno.allow_all {
        // Allow all
        args.push("--allow-all".into())
    } else {
        // Allow env
        add_deno_flag(&mut args, "--allow-env", &config.deno.allow_env);

        // Allow sys
        if config.deno.allow_sys {
            args.push("--allow-sys".into())
        }

        // Allow hrtime
        if config.deno.allow_sys {
            args.push("--allow-sys".into())
        }

        // Allow net
        add_deno_flag(&mut args, "--allow-net", &config.deno.allow_net);

        // Allow ffi
        if config.deno.allow_ffi {
            args.push("--allow-ffi".into())
        }

        // Allow read
        add_deno_flag(&mut args, "--allow-read", &config.deno.allow_read);

        // Allow run
        add_deno_flag(&mut args, "--allow-run", &config.deno.allow_run);

        // Allow write
        add_deno_flag(&mut args, "--allow-write", &config.deno.allow_write);
    }

    for argument in arguments.into_iter() {
        args.push(argument);
    }

    args.push(format!("./{}/gleam.main.mjs", entry.to_string_lossy()));

    ProjectIO::new().exec("deno", &args, &[], None, Stdio::Inherit)
}

fn add_deno_flag(args: &mut Vec<String>, flag: &str, flags: &Vec<String>) {
    if !flags.is_empty() {
        if flags.starts_with(&["*".to_owned()]) {
            args.push(flag.to_owned())
        } else {
            args.push(format!("{}=[{}]", flag.to_owned(), flags.join(" ")))
        }
    }
}
