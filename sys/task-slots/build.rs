use std::{collections::BTreeMap, path::PathBuf};
use std::io::Write;

fn main() {
    println!("cargo::rerun-if-env-changed=HUBRIS_TASK_SLOTS");
    if let Ok(text) = std::env::var("HUBRIS_TASK_SLOTS") {
        let map: BTreeMap<String, usize> = ron::from_str(&text).unwrap();

        let mut path = PathBuf::from(std::env::var("OUT_DIR").unwrap());
        path.push("task_slots.rs");

        let mut f = std::fs::File::create(path).unwrap();

        writeln!(f, "pub struct Slots {{").unwrap();
        for name in map.keys() {
            writeln!(f, "    pub {name}: userlib::TaskId,").unwrap();
        }
        writeln!(f, "}}").unwrap();

        writeln!(f, "pub const SLOTS: Slots = Slots {{").unwrap();
        for (name, index) in map {
            writeln!(f, "    {name}: userlib::TaskId::gen0({index}),").unwrap();
        }
        writeln!(f, "}};").unwrap();
    }
}
