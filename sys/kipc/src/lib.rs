//! API for the kernel's `kipc` interface. This should only be used from
//! supervisor tasks.

#![no_std]

use core::{num::NonZeroUsize, sync::atomic::Ordering};

use hubris_abi::Kipcnum;
pub use hubris_abi::TaskState;

/// Reads the scheduling/fault status of the task with the given index.
///
/// This is powerful, but relatively expensive, because deserializing
/// `TaskState` is somewhat complex. To just detect that a task has faulted, see
/// `find_faulted_task`.
pub fn read_task_status(task_index: usize) -> TaskState {
    let mut response = [0; size_of::<TaskState>()];
    let (_, len) = userlib::sys_send_to_kernel(
        Kipcnum::ReadTaskStatus as u16,
        &(task_index as u32).to_le_bytes(),
        &mut response,
        &mut [],
    );
    match ssmarshal::deserialize(&response[..len]) {
        Ok((state, _)) => state,
        Err(_) => panic!(),
    }
}

/// Scans tasks from `task_index` looking for a task that has failed. Returns
/// its index if found, or `None` if it hits the end of the table.
///
/// This operation is unable to detect that task 0 failed, which is deliberate:
/// it's intended to be called from task 0, the supervisor.
pub fn find_faulted_task(task_index: usize) -> Option<NonZeroUsize> {
    let mut response = [0; 4];
    let (_, _len) = userlib::sys_send_to_kernel(
        Kipcnum::FindFaultedTask as u16,
        &(task_index as u32).to_le_bytes(),
        &mut response,
        &mut [],
    );
    let i = u32::from_le_bytes(response);
    NonZeroUsize::new(i as usize)
}

/// Requests that the task at a given index be reinitialized and optionally
/// started.
///
/// `new_state` controls whether the task gets set to runnable after
/// reinitialization.
pub fn reinitialize_task(task_index: usize, new_state: NewState) {
    let mut msg = [0; 5];
    msg[..4].copy_from_slice(&(task_index as u32).to_le_bytes());
    msg[4] = new_state as u8;

    let _ = userlib::sys_send_to_kernel(
        Kipcnum::RestartTask as u16,
        &msg,
        &mut [],
        &mut [],
    );
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum NewState {
    Halted = 0,
    Runnable = 1,
}

pub fn reset() -> ! {
    userlib::sys_send_to_kernel(Kipcnum::Reset as u16, &[], &mut [], &mut []);
    // The kernel does not return from this, but we currently have no way of
    // indicating that, so...
    loop {
        core::sync::atomic::compiler_fence(Ordering::SeqCst);
    }
}

