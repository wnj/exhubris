use crate::{Lease, TaskDeath, TaskId, ResponseCode, RecvMessage};
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

pub fn sys_reply(
    _sender: TaskId,
    _code: ResponseCode,
    _message: &[u8],
) {
    unimplemented!()
}

