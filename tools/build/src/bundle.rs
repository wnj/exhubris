use indexmap::{IndexMap, IndexSet};
use serde::Serialize;

#[derive(Serialize)]
pub struct AppToml {
    pub target: String,
    pub name: String,
    pub board: String,

    pub kernel: KernelToml,

    pub tasks: IndexMap<String, TaskToml>,
}

#[derive(Serialize)]
pub struct KernelToml {
    pub name: String
}

#[derive(Serialize)]
pub struct TaskToml {
    pub notifications: IndexSet<String>,
}
