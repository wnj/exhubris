#![no_std]
#![no_main]

use userlib::{sys_recv_open, sys_reply, ResponseCode};

#[export_name = "main"]
fn main() -> ! {
    let mut buffer = [core::mem::MaybeUninit::uninit(); 32];
    loop {
        let rm = sys_recv_open(&mut buffer, 0);
        sys_reply(rm.sender, ResponseCode::SUCCESS, &[]);
    }
}
