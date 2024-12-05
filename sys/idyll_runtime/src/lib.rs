#![no_std]

use core::{marker::PhantomData, mem::MaybeUninit};

use userlib::{LeaseAttributes, MessageOrNotification, Message, TaskDeath, TaskId};
pub use userlib::ReplyFaultReason;
use zerocopy::{FromBytes, FromZeros, Immutable, IntoBytes};

/// Implemented by operation enum types.
pub trait ServerOp: TryFrom<u16> {
    const INCOMING_SIZE: usize;
}

#[doc(hidden)]
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

/// Blanket-implemented for servers of an IDL interface defined by op `O`.
/// Implementation detail.
pub trait Server<Op>
    where Op: ServerOp,
{
    fn dispatch_op(&mut self, op: Op, rm: &Message<'_>) -> Result<(), ReplyFaultReason>;
}

/// Receives a single message from any origin, but with notifications filtered
/// out. Dispatches it to `server` and returns.
pub fn dispatch<S, O>(server: &mut S, buffer: &mut [MaybeUninit<u8>])
    where for<'a> (PhantomData<O>, &'a mut S): Server<O>,
          O: ServerOp,
{
    let rm = userlib::sys_recv_open(buffer, 0);
    match rm {
        MessageOrNotification::Message(m) => {
            if let Err(e) = dispatch_inner(server, &m) {
                userlib::sys_reply_fault(m.sender, e);
            }
        }
        MessageOrNotification::Notification(_) => {
            // This statically can't happen, because we passed a zero
            // notification mask. But the compiler can't see that. So if we were
            // to `unreachable!()` we'd get a panic site, and that sucks.
            //
            // Instead, just return.
            //
            // TODO: this could benefit from a `sys_recv` wrapper that has a
            // statically-zero notification mask.
        }
    }
}

/// Receives a single message from any origin, dispatches it to `server` and
/// returns.
///
/// This accepts notifications that match `mask` by calling into `S`'s
/// `NotificationHandler` impl.
pub fn dispatch_or_event<S, O>(server: &mut S, mask: u32, buffer: &mut [MaybeUninit<u8>])
    where for<'a> (PhantomData<O>, &'a mut S): Server<O>,
          O: ServerOp,
          S: NotificationHandler,
{
    let rm = userlib::sys_recv_open(buffer, mask);
    match rm {
        MessageOrNotification::Message(m) => {
            let r = dispatch_inner(server, &m);
            if let Err(e) = r {
                userlib::sys_reply_fault(m.sender, e);
            }
        }
        MessageOrNotification::Notification(bits) => {
            server.handle_notification(bits);
        }
    }
}

/// Implemented by servers that can also process notifications.
pub trait NotificationHandler {
    /// One _or more_ notifications have been posted, as indicated by 1-bits in
    /// `bits`.
    fn handle_notification(&mut self, bits: u32);
}

fn dispatch_inner<S, O>(server: &mut S, rm: &Message<'_>) -> Result<(), ReplyFaultReason>
    where for<'a> (PhantomData<O>, &'a mut S): Server<O>,
          O: ServerOp,
{
    match O::try_from(rm.operation) {
        Err(_) => Err(ReplyFaultReason::UndefinedOperation),
        Ok(op) => {
            (PhantomData, server).dispatch_op(op, rm)?;
            Ok(())
        }
    }
}

/// Implemented by lease attribute types.
pub trait Attribute {
    /// The minimum set of attributes required for a lease to match.
    fn min_attributes() -> LeaseAttributes;
}

/// Implemented by lease attribute types that allow reads.
///
/// Such a type should include `LeaseAttributes::READ` in its `min_attributes`
/// result.
pub trait AttributeRead: Attribute {}

/// Implemented by lease attribute types that allow writes.
///
/// Such a type should include `LeaseAttributes::WRITE` in its `min_attributes`
/// result.
pub trait AttributeWrite: Attribute {}

/// It's technically possible to specify a lease that has no attributes; we use
/// `()` for this instead of an attribute type.
impl Attribute for () {
    fn min_attributes() -> LeaseAttributes {
        LeaseAttributes::empty()
    }
}

/// Lease attribute type marking a lease that can be read from.
///
/// This only appears as a type parameter, never a value.
pub enum Read {}

impl Attribute for Read {
    fn min_attributes() -> LeaseAttributes {
        LeaseAttributes::READ
    }
}
impl AttributeRead for Read {}

/// Lease attribute type marking a lease that can be written to.
///
/// This only appears as a type parameter, never a value.
pub enum Write {}

impl Attribute for Write {
    fn min_attributes() -> LeaseAttributes {
        LeaseAttributes::WRITE
    }
}
impl AttributeWrite for Write {}

/// Lease attribute type marking a lease that can be both read from and written
/// to.
///
/// This only appears as a type parameter, never a value.
pub enum ReadWrite {}

impl Attribute for ReadWrite {
    fn min_attributes() -> LeaseAttributes {
        LeaseAttributes::READ | LeaseAttributes::WRITE
    }
}
impl AttributeRead for ReadWrite {}
impl AttributeWrite for ReadWrite {}

/// A handle to a leased slice of `T` with attributes `A`.
///
/// `A` is an attribute type, generally one of `Read`, `Write`, or `ReadWrite`.
/// This determines which operations are available to the holder of this handle.
///
/// `T` is any `Sized` type, though in practice it needs to implement the
/// `zerocopy` family of byte conversion traits, and should almost certainly be
/// a standard primitive type.
pub struct Leased<A: Attribute, T> {
    lender: TaskId,
    index: usize,
    len: usize,
    _marker: PhantomData<(A, *mut T)>,
}

/// Lease check code factored out, so it doesn't get monomorphized on `A`.
fn check_lease_basics<T>(
    lender: TaskId,
    index: usize,
    min_atts: LeaseAttributes,
) -> Option<usize> {
    let info = userlib::sys_borrow_info(lender, index)?;

    if info.len % size_of::<T>() != 0 {
        // Wrong size 
        return None;
    }

    if !info.atts.contains(min_atts) {
        // Wrong attributes
        return None;
    }

    Some(info.len / size_of::<T>())
}

impl<A: Attribute, T> Leased<A, T> {
    /// Attempts to create a new handle to lease `index` from caller `lender`.
    ///
    /// This will fail, returning `None`, if any of the following conditions
    /// apply:
    /// - The lender has gone away (restarted).
    /// - The lease is not the right size for containing a whole number of `T`s.
    /// - The lease attributes don't grant _at least_ the rights specified by
    ///   `A`.
    pub fn new(lender: TaskId, index: usize) -> Option<Self> {
        let len = check_lease_basics::<T>(lender, index, A::min_attributes())?;

        Some(Self {
            lender,
            index,
            len,
            _marker: PhantomData,
        })
    }

    /// Returns the number of elements in the slice of `T`, cached at creation.
    ///
    /// Because the length is cached, this cannot be used to detect whether the
    /// lender is still there.
    pub fn len(&self) -> usize {
        self.len
    }

    /// Checks if the `len` is 0.
    ///
    /// Because the length is cached, this cannot be used to detect whether the
    /// lender is still there.
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }
}

impl<A, T> Leased<A, T>
    where A: AttributeRead,
          T: FromZeros + FromBytes + IntoBytes,
{
    /// Reads a single element from the leased slice at the given index.
    ///
    /// Attempting to read off the end of the slice will cause a fault; you are
    /// expected to have checked `len()`.
    pub fn read(&self, index: usize) -> Result<T, LenderError> {
        let mut value = T::new_zeroed();
        let offset = index * size_of::<T>();
        let n = userlib::sys_borrow_read(
            self.lender,
            self.index,
            offset,
            value.as_mut_bytes(),
        ).ok_or(LenderError)?;
        if n != size_of::<T>() {
            return Err(LenderError);
        }
        Ok(value)
    }

    /// Reads a range of elements into a buffer, starting at the given index.
    ///
    /// Attempting to read off the end of the slice will cause a fault; you are
    /// expected to have checked `len()`.
    pub fn read_range(
        &self,
        index: usize,
        buf: &mut [T],
    ) -> Result<(), LenderError> {
        let offset = index * size_of::<T>();
        let n = userlib::sys_borrow_read(
            self.lender,
            self.index,
            offset,
            buf.as_mut_bytes(),
        ).ok_or(LenderError)?;
        if n != size_of::<T>() * buf.len() {
            return Err(LenderError);
        }
        Ok(())
    }
}

impl<A, T> Leased<A, T>
    where A: AttributeWrite,
          T: IntoBytes + Immutable,
{
    /// Writes a single element into the leased slice at the given index.
    ///
    /// Attempting to write off the end of the slice will cause a fault; you are
    /// expected to have checked `len()`.
    pub fn write(&self, index: usize, value: T) -> Result<(), LenderError> {
        let offset = index * size_of::<T>();
        let n = userlib::sys_borrow_write(
            self.lender,
            self.index,
            offset,
            value.as_bytes(),
        ).ok_or(LenderError)?;
        if n != size_of::<T>() {
            return Err(LenderError);
        }
        Ok(())
    }

    /// Writes a range of elements into a buffer, starting at the given index.
    ///
    /// Attempting to write off the end of the slice will cause a fault; you are
    /// expected to have checked `len()`.
    pub fn write_range(
        &self,
        index: usize,
        elts: &[T],
    ) -> Result<(), LenderError> {
        let offset = index * size_of::<T>();
        let n = userlib::sys_borrow_write(
            self.lender,
            self.index,
            offset,
            elts.as_bytes(),
        ).ok_or(LenderError)?;
        if n != size_of::<T>() * elts.len() {
            return Err(LenderError);
        }
        Ok(())
    }
}

/// Error produced by lease operations to indicate that the lending task has
/// gone away.
#[derive(Copy, Clone, Debug)]
pub struct LenderError;

pub trait FromTaskDeath {
    fn from_task_death(_: TaskDeath) -> Self;
}

impl<T, E: FromTaskDeath> FromTaskDeath for Result<T, E> {
    fn from_task_death(d: TaskDeath) -> Self {
        Err(E::from_task_death(d))
    }
}
