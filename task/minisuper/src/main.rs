//! Minimal supervisor task.

#![no_std]
#![no_main]

/// This is a fixed detail of the kernel-supervisor interface: the kernel pokes
/// bit 0 on fault.
const FAULT_NOTIFICATION: u32 = 1;

#[export_name = "main"]
fn main() -> ! {
    loop {
        userlib::sys_recv_notification(FAULT_NOTIFICATION);

        // Something has died. Or, someone has posted a notification to us
        // in an attempt to convince us that something has died. Either way.

        // find_faulted_task(i) scans for the first task at an index greater
        // than i that has failed. Since we are task 0, we start out with i=0,
        // skipping ourselves.

        let mut i = 0;
        while let Some(i2) = kipc::find_faulted_task(i) {
            i = usize::from(i2);
            kipc::reinitialize_task(i, kipc::NewState::Runnable);
        }
    }
}
