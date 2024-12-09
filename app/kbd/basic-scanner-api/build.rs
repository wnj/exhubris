use std::path::PathBuf;

fn main() {
    println!("cargo::rerun-if-changed=../basic-scanner/scanner-idl.kdl");
    let iface = hubris_build::idl::load_interface("../basic-scanner/scanner-idl.kdl").unwrap();
    let client = hubris_build::idl::codegen::generate_client(&iface).unwrap();
    let client = hubris_build::idl::codegen::format_code(&client);
    let mut outpath = PathBuf::from(std::env::var("OUT_DIR").unwrap());
    outpath.push("generated_client.rs");

    std::fs::write(&outpath, client).unwrap();

}
