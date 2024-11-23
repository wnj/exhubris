use std::path::PathBuf;
use std::io::Write;

use convert_case::{Case, Casing as _};

fn main() {
    println!("cargo::rerun-if-env-changed=HUBRIS_NOTIFICATIONS");
    if let Ok(text) = std::env::var("HUBRIS_NOTIFICATIONS") {
        let names: Vec<String> = ron::from_str(&text).unwrap();

        let mut path = PathBuf::from(std::env::var("OUT_DIR").unwrap());
        path.push("notifications.rs");

        let mut f = std::fs::File::create(path).unwrap();

        for (i, name) in names.into_iter().enumerate() {
            let const_name = name.to_case(Case::ScreamingSnake);
            writeln!(f, "/// Bit assignment for notification \"{name}\"").unwrap();
            writeln!(f, "pub const {const_name}: u32 = 1 << {i};").unwrap();
        }
    }
}
