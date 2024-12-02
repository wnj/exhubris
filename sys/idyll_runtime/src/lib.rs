#![no_std]

use core::{marker::PhantomData, mem::MaybeUninit};

use userlib::{RecvMessage, TaskId};
pub use userlib::ReplyFaultReason;

#[derive(Copy, Clone, Debug)]
pub enum ReplyFaultOr<E> {
    ReplyFault(ReplyFaultReason),
    Error(E),
}

impl<E> From<E> for ReplyFaultOr<E> {
    fn from(value: E) -> Self {
        Self::Error(value)
    }
}

pub trait ServerOp: TryFrom<u16> {
    const INCOMING_SIZE: usize;
}

pub const fn const_max(sizes: &[usize]) -> usize {
    let mut max = sizes[0];
    let mut i = 1;
    while i < sizes.len() {
        if sizes[i] > max {
            max = sizes[i];
        }
        i += 1;
    }
    max
}

pub trait Server<Op>
    where Op: ServerOp,
{
    fn dispatch_op(&mut self, op: Op, rm: &RecvMessage<'_>) -> Result<(), ReplyFaultReason>;
}

pub fn dispatch<S, O>(server: &mut S, buffer: &mut [MaybeUninit<u8>])
    where for<'a> (PhantomData<O>, &'a mut S): Server<O>,
          O: ServerOp,
{
    let rm = userlib::sys_recv_open(buffer, 0);
    if let Err(e) = dispatch_inner(server, &rm) {
        userlib::sys_reply_fault(rm.sender, e);
    }
}

pub fn dispatch_or_event<S, O>(server: &mut S, mask: u32, buffer: &mut [MaybeUninit<u8>])
    where for<'a> (PhantomData<O>, &'a mut S): Server<O>,
          O: ServerOp,
          S: NotificationHandler,
{
    let rm = userlib::sys_recv_open(buffer, mask);
    if rm.sender == TaskId::KERNEL {
        server.handle_notification(rm.operation_or_notification);
    } else {
        let r = dispatch_inner(server, &rm);
        if let Err(e) = r {
            userlib::sys_reply_fault(rm.sender, e);
        }
    }
}

pub trait NotificationHandler {
    fn handle_notification(&mut self, bits: u32);
}

fn dispatch_inner<S, O>(server: &mut S, rm: &RecvMessage<'_>) -> Result<(), ReplyFaultReason>
    where for<'a> (PhantomData<O>, &'a mut S): Server<O>,
          O: ServerOp,
{
    match O::try_from(rm.operation_or_notification as u16) {
        Err(_) => Err(ReplyFaultReason::UndefinedOperation),
        Ok(op) => {
            (PhantomData, server).dispatch_op(op, rm)?;
            Ok(())
        }
    }
}
