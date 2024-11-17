use crate::{Lease, TaskDeath, TaskId, ResponseCode, RecvMessage};

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
    _incoming: &mut [u8],
    _notification_mask: u32,
    _from: Option<TaskId>,
) -> Result<RecvMessage, TaskDeath> {
    unimplemented!()
}

pub fn sys_recv_open(
    _incoming: &mut [u8],
    _notification_mask: u32,
) -> RecvMessage {
    unimplemented!()
}

pub fn sys_reply(
    _sender: TaskId,
    _code: ResponseCode,
    _message: &[u8],
) {
    unimplemented!()
}

