//! This crate allows build scripts for Hubris task to perform application
//! introspection.

use indexmap::IndexSet;
use serde::{Deserialize, de::DeserializeOwned, Serialize};

/// Retrieved information about a named task.
#[derive(Serialize, Deserialize)]
pub struct TaskInfo {
    /// Index of the task in the application, used to create `TaskId`s.
    index: usize,
    /// The application's defined notification bits, ordered starting from zero.
    notifications: IndexSet<String>,
}

impl TaskInfo {
    pub fn get_index(&self) -> usize {
        self.index
    }

    pub fn notification_mask(&self, name: &str) -> Option<u32> {
        self.notifications.get_index_of(name).map(|i| 1 << i)
    }
}

/// Retrieves the info record for a named task from the build environment.
///
/// Returns `None` if the task does not exist.
///
/// This will emit a Cargo build script directive to stdout, ensuring that your
/// build script will be re-run if the task's info record changes.
pub fn get_task_info(name: &str) -> Option<TaskInfo> {
    let varname = format!("HUBRIS_TASK_INFO_{name}");
    let text = std::env::var(&varname).ok()?;
    println!("cargo::rerun-if-env-changed={varname}");
    Some(serde_json::from_str(&text).expect("bad JSON from build system"))
}

/// Collects the `config` section for the current task, as JSON, and
/// deserializes it into a `T`.
pub fn get_task_config<T>() -> T
    where T: DeserializeOwned,
{
    let text = std::env::var("HUBRIS_TASK_CONFIG")
        .expect("required task config section not provided");
    println!("cargo::rerun-if-env-changed=HUBRIS_TASK_CONFIG");
    serde_json::from_str(&text).expect("config failed to deserialize (is probably wrong)")
}

/// Collects the chip configuration information from the build environment and
/// exposes it as a set of `hubris_chip` cfgs.
pub fn export_chip_cfg() {
    println!("cargo::rerun-if-env-changed=HUBRIS_CHIP_COMPAT");
    println!("cargo::rustc-check-cfg=cfg(hubris_chip, values(any()))");
    if let Ok(text) = std::env::var("HUBRIS_CHIP_COMPAT") {
        for compat_name in text.split(',') {
            println!("cargo::rustc-cfg=hubris_chip=\"{compat_name}\"");
        }
    }
}
