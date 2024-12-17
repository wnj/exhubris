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
        // than or equal to i that has failed. Since we are task 0, we start out
        // with i=1, skipping ourselves.

        let mut next_task = 1;
        while let Some(fault_index) = kipc::find_faulted_task(next_task) {
            let fault_index = usize::from(fault_index);
            kipc::reinitialize_task(fault_index, kipc::NewState::Runnable);
            // Keep moving. Because of the API guarantees on the `fault_index`
            // value from the kernel, we can use wrapping add to advance this
            // without risk of it actually wrapping, to avoid a panic when
            // overflow checks are enabled.
            next_task = fault_index.wrapping_add(1);
        }
    }
}
