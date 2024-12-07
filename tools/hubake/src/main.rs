use std::{path::{Path, PathBuf}, process::Command};

use anyhow::{bail, Context as _, anyhow};
use base64::Engine;
use serde::Deserialize;

fn main() -> anyhow::Result<()> {
    let args = std::env::args().skip(1).collect::<Vec<_>>();

    match args.first().map(|s| s.as_str()) {
        Some("help" | "--help" | "-h") => {
            println!("\
                hubake <subcommand>\n\
                \n\
                where <subcommand> is one of:\n\
                    setup [-f] [<PROJECT>]\n\
                \n\
                    Sets up the Hubris tools for the precise version needed for a project.\n\
                    The project is defined by a hubris-env.toml file, which also marks its\n\
                    root directory. If <PROJECT> is not given as an argument, hubake will\n\
                    search from the current directory up.\n\
                \n\
                    -f  --force-reinstall   rebuild tools even if they appear OK\n\
                \n\
                    help\n\
                \n\
                    Shows this message and exits with an error code.");
            std::process::exit(1);
        }
        Some("setup") => {
            let mut given_root = None;
            std::env::current_dir()
                    .context("can't get current directory?")?;
            let mut force_reinstall = false;
            for arg in &args[1..] {
                match arg.as_str() {
                    "--force-install" | "-f" => force_reinstall = true,
                    _ => {
                        if given_root.is_some() {
                            bail!("too many arguments to setup command");
                        } else {
                            given_root = Some(PathBuf::from(arg));
                        }
                    }
                }
            }

            let storage = prepare_storage_dir()?;
            let given_root = if let Some(r) = given_root {
                r
            } else {
                std::env::current_dir()
                    .context("can't get current directory?")?
            };
            let project_root = find_project_root(&given_root)?;
            println!("checking setup in project root: {}", project_root.display());
            let config = load_config(&project_root)?;

            let _cmd = prepare_run_command(&project_root, &config, &storage, force_reinstall, true)?;

            println!("setup complete");
        }
        _ => {
            // passthrough
            let storage = prepare_storage_dir()?;
            let given_root = std::env::current_dir()
                    .context("can't get current directory?")?;
            let project_root = find_project_root(&given_root)?;
            let config = load_config(&project_root)?;
            let mut cmd = prepare_run_command(&project_root, &config, &storage, false, false)?;

            cmd.args(args);

            cmd.env("HUBRIS_PROJECT_ROOT", &project_root);

            let status = cmd.status()?;
            if status.success() {
                return Ok(());
            } else {
                std::process::exit(2);
            }
        }
    }

    Ok(())
}

fn prepare_run_command(
    project_root: &Path,
    config: &Config,
    storage: &Path,
    force_reinstall: bool,
    be_chatty: bool,
) -> anyhow::Result<Command> {
    let strat = match &config.system {
        SystemConfig::Git { repo, rev } => {
            if be_chatty {
                println!("system: git {repo}");
                println!("   rev: {rev}");
            }

            let toolsdir = storage.join("git")
                .join(base64::prelude::BASE64_STANDARD.encode(repo))
                .join(rev);
            let binary_path = toolsdir.join("bin").join("hubris-build");

            ExecStrategy::CargoInstall {
                binary_path,
                flags_if_needed: vec![
                    "--root".to_string(),
                    toolsdir.display().to_string(),
                    "--git".to_string(),
                    repo.clone(),
                    "--locked".to_string(),
                    "--rev".to_string(),
                    rev.clone(),
                    "hubris-build".to_string(),
                    "--bin".to_string(),
                    "hubris-build".to_string(),

                ],
            }
        }
        SystemConfig::Here => {
            if be_chatty {
                println!("system: in project root");
            }
            ExecStrategy::DirectRun {
                package_name: "hubris-build".to_string(),
                bin_name: "hubris-build".to_string(),
            }
        }
    };

    let toolcmd = match strat {
        ExecStrategy::CargoInstall { binary_path, flags_if_needed } => {
            if !force_reinstall && std::fs::exists(&binary_path)
                .with_context(|| format!("can't check existence of tool at {}", binary_path.display()))?
            {
                if be_chatty {
                    println!("tool already built");
                }
            } else {
                if force_reinstall {
                    println!("skipped checking tool install, --force-reinstall set");
                }
                println!("building tool for rev, this may take a bit...");
                let mut cmd = Command::new("cargo");
                cmd.args(["install", "-q", "-f"]);
                for arg in flags_if_needed {
                    cmd.arg(arg);
                }
                let stat = cmd.status().context("cargo build failed, see output")?;
                if stat.success() {
                    println!("Installed tool");
                } else {
                    bail!("cargo build failed, see output");
                }
            }

            Command::new(binary_path)
        }
        ExecStrategy::DirectRun { package_name, bin_name } => {
            let mut cmd = Command::new("cargo");
            cmd.current_dir(project_root);
            cmd.args(["run", "-q", "-p", &package_name, "--bin", &bin_name]);
            cmd.arg("--");
            cmd
        }
    };
    Ok(toolcmd)
}

enum ExecStrategy {
    CargoInstall {
        binary_path: PathBuf,
        flags_if_needed: Vec<String>,
    },
    DirectRun {
        package_name: String,
        bin_name: String,
    },
}

fn prepare_storage_dir() -> anyhow::Result<PathBuf> {
    let path = dirs::data_dir().ok_or_else(|| anyhow!("can't get data directory for user?"))?
        .join("hubris");

    std::fs::create_dir_all(&path)
        .with_context(|| format!("can't create storage directory at path: {}", path.display()))?;

    let git_dir = path.join("git");
    std::fs::create_dir_all(&git_dir)
        .with_context(|| format!("can't create storage directory at path: {}", git_dir.display()))?;

    Ok(path)
}

fn find_project_root(orig_path: impl AsRef<Path>) -> anyhow::Result<PathBuf> {
    let orig_path = orig_path.as_ref();
    let mut pathbuf = orig_path.canonicalize()
        .with_context(|| format!("can't interpret path: {}", orig_path.display()))?;

    loop {
        let config_path = pathbuf.join("hubris-env.toml");
        if std::fs::exists(&config_path).with_context(|| format!("can't check existence of path: {}", config_path.display()))? {
            return Ok(pathbuf);
        }

        if !pathbuf.pop() {
            break;
        }
    }

    bail!("unable to find a hubris-env.toml in {} or any parent directory", orig_path.display())
}

fn load_config(root: impl AsRef<Path>) -> anyhow::Result<Config> {
    let root = root.as_ref();
    let config_path = root.join("hubris-env.toml");
    let config_text = std::fs::read_to_string(&config_path)
        .with_context(|| format!("can't read config file at path: {}", config_path.display()))?;

    toml::from_str(&config_text)
        .with_context(|| format!("can't parse config file at path: {}", config_path.display()))
}

#[derive(Deserialize, Debug)]
struct Config {
    system: SystemConfig,
}

#[derive(Deserialize, Debug)]
#[serde(tag = "source")]
#[serde(rename_all = "kebab-case")]
enum SystemConfig {
    Git {
        repo: String,
        rev: String,
    },
    Here,
}
