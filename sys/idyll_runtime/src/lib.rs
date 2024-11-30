#![no_std]

use core::{marker::PhantomData, mem::MaybeUninit};

use userlib::RecvMessage;
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

pub trait Server<Op>
    where Op: TryFrom<u16>,
{
    fn dispatch_op(&mut self, op: Op, rm: &RecvMessage<'_>) -> Result<(), ReplyFaultReason>;
}

pub fn dispatch<S, O>(server: &mut S, buffer: &mut [MaybeUninit<u8>])
    where for<'a> (PhantomData<O>, &'a mut S): Server<O>,
          O: TryFrom<u16>,
{
    let rm = userlib::sys_recv_open(buffer, 0);
    if let Err(e) = dispatch_inner(server, &rm) {
        userlib::sys_reply_fault(rm.sender, e);
    }
}

fn dispatch_inner<S, O>(server: &mut S, rm: &RecvMessage<'_>) -> Result<(), ReplyFaultReason>
    where for<'a> (PhantomData<O>, &'a mut S): Server<O>,
          O: TryFrom<u16>,
{
    match O::try_from(rm.operation_or_notification as u16) {
        Err(_) => Err(ReplyFaultReason::UndefinedOperation),
        Ok(op) => {
            (PhantomData, server).dispatch_op(op, rm)?;
            Ok(())
        }
    }
}
