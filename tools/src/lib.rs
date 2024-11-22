pub mod appcfg;

use std::{path::PathBuf, process::Command};

use cargo_metadata::Package;
use miette::{bail, miette, IntoDiagnostic as _, SourceSpan};

/// Definition of a task in an application image.
#[derive(Clone, Debug)]
pub struct TaskDef {
    /// How to find the task's implementation.
    pub source: PackageSource,
    /// Stack size. TODO this will become optional.
    pub stack_size: u64,
}

/// Different ways to get a package.
#[derive(Clone, Debug)]
pub enum PackageSource {
    /// Use a crate with the given name in the workspace containing the app
    /// config.
    WorkspaceCrate(String, SourceSpan),
}

/// Definition of a region of address space with certain properties.
///
/// This appears in two places: in the definition of allocatable memory regions
/// in an SoC, and in the definition of memory-mapped peripherals.
#[derive(Clone, Debug)]
pub struct RegionDef {
    /// Base address of the region.
    pub base: u64,
    /// Size of the region in bytes.
    pub size: u64,
    /// Properties of the region.
    pub attrs: RegionAttrs,
}

#[derive(Copy, Clone, Debug)]
pub struct RegionAttrs {
    pub read: bool,
    pub write: bool,
    pub execute: bool,
    pub device: bool,
}

///////

#[derive(Clone, Debug)]
pub struct BuildEnv {
    /// release of toolchain
    pub release: String,
    /// sysroot for toolchain obtained from rustup.
    pub sysroot: PathBuf,
    /// Host triple --- the triple used for tools that run on the host, rather
    /// than being cross-compiled.
    pub host_triple: String,
    /// The location we have inferred for the linker.
    pub linker_path: PathBuf,
}

pub fn determine_build_env() -> miette::Result<BuildEnv> {
    let sysroot = Command::new("rustc")
        .args(["--print", "sysroot"])
        .output().into_diagnostic()?;
    if !sysroot.status.success() {
        panic!("Could not find execute rustc to get sysroot");
    }
    let sysroot = PathBuf::from(std::str::from_utf8(&sysroot.stdout).into_diagnostic()?.trim());

    let host = Command::new(sysroot.join("bin").join("rustc"))
        .arg("-vV")
        .output().into_diagnostic()?;
    if !host.status.success() {
        panic!("Could not execute rustc to get host");
    }
    let output = std::str::from_utf8(&host.stdout).into_diagnostic()?;
    let host_triple = output.lines()
        .find_map(|line| line.strip_prefix("host: "))
        .ok_or_else(|| miette!("Could not get host from rustc"))?
        .to_string();
    let release = output.lines()
        .find_map(|line| line.strip_prefix("release: "))
        .ok_or_else(|| miette!("Could not get release from rustc"))?
        .to_string();

    let mut linker_path = sysroot.clone();
    linker_path.extend([
        "lib",
        "rustlib",
        &host_triple,
        "bin",
        "gcc-ld",
        "ld.lld",
    ]);
    if !std::fs::exists(&linker_path).into_diagnostic()? {
        bail!("linker not available at: {}", linker_path.display());
    }

    Ok(BuildEnv {
        release,
        sysroot,
        host_triple,
        linker_path,
    })
}

pub enum BuildablePackage {
    WorkspaceCrate {
        name: String,
        /// The entire package metadata record, proving that we found it (and
        /// providing useful contents).
        package: Package,
        /// The cargo arguments to build the crate.
        cargo_args: Vec<String>,
        /// The path where we expect to find the result.
        product_path: PathBuf,
    },
}

