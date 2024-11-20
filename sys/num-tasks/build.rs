use std::path::PathBuf;

fn main() {
    let names = std::env::var("HUBRIS_TASKS").unwrap();
    let task_count = names.split(',').count();

    let mut path = PathBuf::from(std::env::var("OUT_DIR").unwrap());
    path.push("task_count.txt");
    std::fs::write(path, format!("{task_count}")).unwrap();

    println!("cargo::rerun-if-env-changed=HUBRIS_TASKS");
}
