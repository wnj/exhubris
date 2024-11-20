#![no_std]
#![no_main]

use userlib::{sys_recv_open, sys_reply, ResponseCode};

#[no_mangle]
static mut MESSAGE_COUNT: u32 = 0;

#[export_name = "main"]
fn main() -> ! {
    let mut buffer = [core::mem::MaybeUninit::uninit(); 32];
    loop {
        let rm = sys_recv_open(&mut buffer, 0);
        unsafe {
            MESSAGE_COUNT = MESSAGE_COUNT.wrapping_add(1);
        }
        sys_reply(rm.sender, ResponseCode::SUCCESS, &[]);
    }
}
