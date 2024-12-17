use std::io::Write;
use std::path::PathBuf;

use serde::Deserialize;

fn main() {
    let outpath = PathBuf::from(std::env::var("OUT_DIR").unwrap());

    println!("cargo::rerun-if-changed=usbhid-idl.kdl");
    let iface = hubris_build::idl::load_interface("usbhid-idl.kdl").unwrap();
    let server = hubris_build::idl::codegen::generate_server(&iface).unwrap();
    let server = hubris_build::idl::codegen::format_code(&server);
    let genserver_path = outpath.join("generated_server.rs");

    std::fs::write(&genserver_path, server).unwrap();

    let config: Config = hubris_build_util::get_task_config();
    let Some((task_name, not_name)) = config.on_event.split_once('#') else {
        panic!("keyboard signal name should be 'task#notification' but was: {}",
            config.on_event);
    };
    let Some(evt_task_info) = hubris_build_util::get_task_info(task_name) else {
        panic!("keyboard signal name references unknown task {task_name}");
    };
    let Some(evt_mask) = evt_task_info.notification_mask(not_name) else {
        panic!("keyboard signal name references unknown notification {not_name}");
    };

    let Some((task_name, not_name)) = config.on_report.split_once('#') else {
        panic!("keyboard signal name should be 'task#notification' but was: {}",
            config.on_report);
    };
    let Some(report_task_info) = hubris_build_util::get_task_info(task_name) else {
        panic!("keyboard signal name references unknown task {task_name}");
    };
    let Some(report_mask) = evt_task_info.notification_mask(not_name) else {
        panic!("keyboard signal name references unknown notification {not_name}");
    };

    let mut config_out = std::fs::File::create(outpath.join("usb_config.rs")).unwrap();
    writeln!(config_out, "const EVENT_TASK_INDEX: u16 = {};", evt_task_info.get_index()).unwrap();
    writeln!(config_out, "const EVENT_NOTIFICATION_MASK: u32 = {};", evt_mask).unwrap();
    writeln!(config_out, "const REPORT_TASK_INDEX: u16 = {};", report_task_info.get_index()).unwrap();
    writeln!(config_out, "const REPORT_NOTIFICATION_MASK: u32 = {};", report_mask).unwrap();
    drop(config_out);
}

#[derive(Deserialize)]
struct Config {
    on_event: String,
    on_report: String,
}
