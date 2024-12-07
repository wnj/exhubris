#![no_std]
#![no_main]

use userlib::{sys_panic, sys_recv_msg_open, sys_reply, ResponseCode};

#[no_mangle]
static mut MESSAGE_COUNT: u32 = 0;

#[export_name = "main"]
fn main() -> ! {
    let mut buffer = [core::mem::MaybeUninit::uninit(); 32];
    loop {
        let rm = sys_recv_msg_open(&mut buffer);
        unsafe {
            MESSAGE_COUNT = MESSAGE_COUNT.wrapping_add(1);
        }
        sys_reply(rm.sender, ResponseCode::SUCCESS, &[]);

        // Periodically crash this task to test both supervisor restarts, and
        // IPC client handling of dead codes.
        if unsafe { MESSAGE_COUNT } % 10 == 0 {
            sys_panic(b"");
        }
    }
}
