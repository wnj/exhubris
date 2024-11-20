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
    // TODO: we're assuming pong is at task index 1, which is not true in the
    // general case.
    let mut pong = TaskId::gen0(1);

    // Arbitrarily chosen operation code:
    let ping_op: u16 = 1;
    // Very important message:
    let message = b"I am the lizard king";

    // We do not expect a response.
    let mut incoming = [];
    // We don't lease any memory.
    let mut leases = [];

    // Record the current time so we can start our delay loop properly.
    let mut next_send = userlib::sys_get_timer().now;

    const INTERVAL: u64 = 5;

    loop {
        userlib::sys_set_timer(Some(next_send), 1);

        // The proper thing to do, when waiting for a timer, is to sleep waiting
        // for notifications _and then check the time._ Otherwise other tasks
        // can wake you up by posting.
        loop {
            userlib::sys_recv_notification(1);
            let now = userlib::sys_get_timer().now;
            if now >= next_send {
                next_send += INTERVAL;
                break;
            }
        }

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
