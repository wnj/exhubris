//! A task that spams its counterpart, pong, with IPC messages.
//!
//! This task exists to test the build system and IPC implementation. You
//! probably don't want to include it in your application, particularly because
//! it doesn't sleep.

#![no_std]
#![no_main]

use userlib::TaskId;

#[export_name = "main"]
fn main() -> ! {
    // TODO: we're assuming pong is at task index 0, which is not true in the
    // general case.
    let mut pong = TaskId::gen0(0);

    // Arbitrarily chosen operation code:
    let ping_op: u16 = 1;
    // Very important message:
    let message = b"I am the lizard king";

    // We do not expect a response.
    let mut incoming = [];
    // We don't lease any memory.
    let mut leases = [];

    loop {
        let r = userlib::sys_send(
            pong,
            ping_op,
            message,
            &mut incoming,
            &mut leases,
        );

        match r {
            Ok((_retval, _response_len)) => {
                // TODO consider panicking here if the results are wrong, to
                // help test the syscall interface?
            }
            Err(dead) => {
                // Update generation to handle pong crash
                pong = pong.with_generation(dead.new_generation());
            }
        }
    }
}
