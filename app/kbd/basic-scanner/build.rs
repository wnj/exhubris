use std::io::Write;
use std::path::PathBuf;

use serde::Deserialize;

fn main() {
    let outpath = PathBuf::from(std::env::var("OUT_DIR").unwrap());

    println!("cargo::rerun-if-changed=scanner-idl.kdl");
    let iface = hubris_build::idl::load_interface("scanner-idl.kdl").unwrap();
    let server = hubris_build::idl::codegen::generate_server(&iface).unwrap();
    let server = hubris_build::idl::codegen::format_code(&server);
    let genserver_path = outpath.join("generated_server.rs");

    std::fs::write(&genserver_path, server).unwrap();

    let config: Config = hubris_build_util::get_task_config();
    let Some((task_name, not_name)) = config.keyboard.split_once('#') else {
        panic!("keyboard signal name should be 'task#notification' but was: {}",
            config.keyboard);
    };
    let Some(task_info) = hubris_build_util::get_task_info(task_name) else {
        panic!("keyboard signal name references unknown task {task_name}");
    };
    let Some(mask) = task_info.notification_mask(not_name) else {
        panic!("keyboard signal name references unknown notification {not_name}");
    };

    let mut config_out = std::fs::File::create(outpath.join("config.rs")).unwrap();
    writeln!(config_out, "const KEYBOARD_TASK_INDEX: u16 = {};", task_info.get_index()).unwrap();
    writeln!(config_out, "const KEYBOARD_NOTIFICATION_MASK: u32 = {};", mask).unwrap();
    drop(config_out);
}

#[derive(Deserialize)]
struct Config {
    keyboard: String,
}
