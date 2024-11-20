use crate::{Lease, RecvMessage, ResponseCode, TaskDeath, TaskId, TimerSettings, ReplyFaultReason};
use core::mem::MaybeUninit;

pub(crate) fn idle() {
    unimplemented!();
}

pub fn sys_panic(_msg: &[u8]) -> ! {
    unimplemented!();
}

pub fn sys_send(
    _target: TaskId,
    _operation: u16,
    _outgoing: &[u8],
    _incoming: &mut [u8],
    _leases: &mut [Lease<'_>],
) -> Result<(ResponseCode, usize), TaskDeath> {
    unimplemented!()
}


pub fn sys_recv(
    _incoming: &mut [MaybeUninit<u8>],
    _notification_mask: u32,
    _from: Option<TaskId>,
) -> Result<RecvMessage<'_>, TaskDeath> {
    unimplemented!()
}

pub fn sys_recv_open(
    _incoming: &mut [MaybeUninit<u8>],
    _notification_mask: u32,
) -> RecvMessage<'_> {
    unimplemented!()
}

pub fn sys_recv_notification(
    _notification_mask: u32,
) -> u32 {
    unimplemented!()
}

pub fn sys_reply(
    _sender: TaskId,
    _code: ResponseCode,
    _message: &[u8],
) {
    unimplemented!()
}

pub fn sys_reply_fault(
    _sender: TaskId,
    _reason: ReplyFaultReason,
) {
    unimplemented!()
}

pub fn sys_set_timer(
    _deadline: Option<u64>,
    _bitmask: u32,
) {
    unimplemented!()
}

pub fn sys_get_timer() -> TimerSettings {
    unimplemented!()
}
