#![no_std]
#![allow(unexpected_cfgs)] // for now

pub enum Sysnum {
    Send = 0,
    Recv = 1,
    Reply = 2,
    SetTimer = 3,
    IrqControl = 7,
    Panic = 8,
    GetTimer = 9,
    ReplyFault = 12,
}

pub const GEN_BITS: u32 = 6;

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub struct TaskId(u16);

impl TaskId {
    pub const KERNEL: TaskId = Self(!0);

    pub const fn new(task_index: u16, generation: Gen) -> Self {
        Self(task_index & ((1 << GEN_BITS) - 1) | ((generation.0 as u16) << (16 - GEN_BITS)))
    }

    pub const fn gen0(task_index: u16) -> Self {
        Self::new(task_index, Gen::DEFAULT)
    }

    pub const fn with_generation(self, generation: Gen) -> Self {
        Self::new(self.task_index(), generation)
    }

    pub const fn task_index(self) -> u16 {
        self.0 & ((1 << GEN_BITS) - 1)
    }
}

impl From<TaskId> for u16 {
    fn from(tid: TaskId) -> u16 {
        tid.0
    }
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Default)]
pub struct Gen(u8);

impl Gen {
    pub const DEFAULT: Self = Self(0);
}

#[repr(transparent)]
pub struct Lease<'a> {
    inner: AbiLease,
    _marker: PhantomData<&'a mut ()>,
}

#[repr(C)]
struct AbiLease {
    attributes: u32,
    base_address: *const u8,
    length: usize,
}

pub fn idle() {
    arch::idle();
}

cfg_if::cfg_if! {
    if #[cfg(any(
        hubris_target = "thumbv7em-none-eabihf",
        hubris_target = "thumbv6m-none-eabi",
    ))] {
        #[path = "arch/arm_m.rs"]
        mod arch;
    } else {
        #[path = "arch/fake.rs"]
        mod arch;
    }
}

use core::cell::Cell;
use core::marker::PhantomData;

#[doc(inline)]
pub use self::arch::sys_panic;

#[doc(inline)]
pub use self::arch::sys_send;

#[doc(inline)]
pub use self::arch::sys_send_to_kernel;

#[doc(inline)]
pub use self::arch::sys_recv;

#[doc(inline)]
pub use self::arch::sys_recv_open;

#[doc(inline)]
pub use self::arch::sys_recv_notification;

#[doc(inline)]
pub use self::arch::sys_reply;

#[doc(inline)]
pub use self::arch::sys_reply_fault;

#[doc(inline)]
pub use self::arch::sys_set_timer;

#[doc(inline)]
pub use self::arch::sys_get_timer;

#[doc(inline)]
pub use self::arch::sys_enable_irq;

#[derive(Copy, Clone, Debug)]
pub struct ResponseCode(u32);

impl ResponseCode {
    pub const SUCCESS: Self = Self(0);
}

#[derive(Copy, Clone, Debug)]
pub struct TaskDeath {
    now: Gen,
}

impl TaskDeath {
    pub fn new_generation(self) -> Gen {
        self.now
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct NotADeadCode;

impl TryFrom<ResponseCode> for TaskDeath {
    type Error = NotADeadCode;
    fn try_from(rc: ResponseCode) -> Result<Self, Self::Error> {
        if rc.0 & 0xFFFF_FF00 == 0xFFFF_FF00 {
            Ok(TaskDeath { now: Gen(rc.0 as u8) })
        } else {
            Err(NotADeadCode)
        }
    }
}

#[derive(Debug)]
pub struct RecvMessage<'a> {
    pub sender: TaskId,
    pub operation_or_notification: u32,
    pub data: Result<&'a mut [u8], Truncated>,
    pub reply_capacity: usize,
    pub lease_count: usize,
}

#[derive(Copy, Clone, Debug)]
pub struct Truncated;

#[derive(Copy, Clone, Debug)]
pub struct TimerSettings {
    pub now: u64,
    pub alarm: Option<(u64, u32)>,
}

#[derive(Copy, Clone, Debug)]
pub enum ReplyFaultReason {
    /// The message indicated some operation number that is unknown to the
    /// server -- which almost certainly indicates that the client intended the
    /// message for a different kind of server.
    UndefinedOperation = 0,
    /// The message sent by the client had the wrong size to even attempt
    /// parsing by the server -- either too short or too long. (Because most
    /// messages are fixed size, it currently doesn't seem useful to distinguish
    /// between too-short and too-long.)
    BadMessageSize = 1,
    /// The server attempted to parse the message, and couldn't. This may
    /// indicate an enum with an illegal value, or a more nuanced error on
    /// operations that use serde encoding.
    BadMessageContents = 2,
    /// The client did not provide the leases required for the operation, or
    /// provided them with the wrong attributes.
    BadLeases = 3,
    /// The client did not provide a reply buffer large enough to receive the
    /// server's reply, despite this information being implied by the IPC
    /// protocol.
    ReplyBufferTooSmall = 4,

    /// Application-defined: The client attempted to operate on a resource that
    /// is not available to them due to mandatory access control or other type
    /// of access validation.
    AccessViolation = 5,
}

cfg_if::cfg_if! {
    if #[cfg(feature = "no-panic")] {

        // The "panic handler" that won't link, ensuring that no implicit panics
        // occur.
        #[panic_handler]
        fn panic(_: &core::panic::PanicInfo<'_>) -> !{
            extern "C" {
                fn you_have_introduced_a_panic_but_this_task_has_disabled_panics() -> !;
            }

            // Safety: this function doesn't exist, deliberately. We call it to
            // cause a link error. So, while it _appears_ unsafe here, there is
            // no contract we need to meet.
            unsafe {
                you_have_introduced_a_panic_but_this_task_has_disabled_panics()
            }
        }

    } else if #[cfg(feature = "no-panic-messages")] {

        // Panic with a minimal string, rather than formatting the error
        // message. When combined with LTO this optimizes out all the panic
        // format machinery.
        #[panic_handler]
        fn panic(_: &core::panic::PanicInfo<'_>) -> ! {
            sys_panic(b"PANIC")
        }

    } else {

        // Relatively expensive panic handler that actually formats the output.
        #[panic_handler]
        fn panic(info: &core::panic::PanicInfo<'_>) -> ! {
            // Theory of operation:
            //
            // So, as you might have inferred from the name of this function,
            // we're panicking right now. Because we're panicking, it is pretty
            // darn important that we don't panic _more._
            //
            // That's impossible to guarantee, because rendering a panic message
            // can involve calling arbitrary Display impls of arbitrary types,
            // which could panic. So we'll try our best not to introduce panics
            // _here_ and hope the caller doesn't write a panicking Display
            // impl. If they do, we stop the process with the message "recursive
            // panic."
            //
            // It's also important that we not run out of stack. That is _also_
            // impossible to prevent in all cases, particularly if the panic
            // message calls a Debug impl, which tend to be naturally recursive.
            // (This is what `unwrap` does by default.) But we can at least try
            // to avoid making things _worse,_ by using statically allocated
            // memory for most of our state, instead of putting it on the stack.
            //
            // Below, we have a very basic implementation of core::fmt::Write
            // that tries really hard not to panic, and not to put too much on
            // the stack.

            use core::fmt::Write;
            use core::sync::atomic::{AtomicBool, Ordering};

            static IN_PANIC: AtomicBool = AtomicBool::new(false);

            // Check for recursive panic and stop the process if it happens. We
            // are not using a swap here because we know we're in a
            // single-threaded context, and swap would require a polyfill on
            // some of our target platforms.
            if IN_PANIC.load(Ordering::Relaxed) {
                sys_panic(b"recursive panic");
            }
            IN_PANIC.store(true, Ordering::Relaxed);

            /// Number of UTF-8 bytes (not characters!) reserved for panic
            /// messages. Any panic message that exceeds this will be truncated,
            /// and thus might not be valid UTF-8. Larger values burn more
            /// static RAM.
            ///
            /// I suggest keeping this power-of-two-minus-1 so that it plus the
            /// IN_PANIC flag are a convenient power of two.
            const PANIC_MESSAGE_MAX: usize = 127;
            /// The actual reserved buffer.
            static mut PANIC_BUFFER: [u8; PANIC_MESSAGE_MAX] = [0; PANIC_MESSAGE_MAX];

            /// A writer that spits bytes into a limited-length buffer.
            struct LimitedWriter {
                /// The output.
                buf: &'static mut [u8; PANIC_MESSAGE_MAX],
                /// The position, which may be past the end of the buffer, in
                /// which case we don't produce more output.
                pos: usize,
            }

            impl Write for LimitedWriter {
                fn write_str(&mut self, s: &str) -> core::fmt::Result {
                    // If the buffer is already full, this is a no-op.
                    //
                    // We're using `>=` here even though we don't expect pos to
                    // _pass_ buf.len(), because we're about to do some unsafe
                    // and it'd be a real shame to access off the end of the
                    // buffer.
                    if self.pos >= self.buf.len() {
                        return Ok(());
                    }

                    // Slice the remaining section of the buffer.
                    //
                    // Safety: we have checked the range above.
                    let remaining = unsafe { self.buf.get_unchecked_mut(self.pos..) };

                    // Compute the size of the prefix of `s` that we can write
                    // into our remaining slice.
                    let strbytes = s.as_bytes();
                    let to_write = usize::min(remaining.len(), strbytes.len());

                    // Copy the bytes. We are not using `slice::copy_from_slice`
                    // because it contains panics, and while the compiler can
                    // _usually_ eliminate them, that's a tenuous guarantee.
                    //
                    // Safety: Both pointers are valid since we're deriving them
                    // from slices we hold. `to_write` is the minimum of the two
                    // lengths. So, this should be sound.
                    unsafe {
                        core::ptr::copy_nonoverlapping(
                            strbytes.as_ptr(),
                            remaining.as_mut_ptr(),
                            to_write,
                        );
                    }

                    // Advance our position. We use a wrapping add here to
                    // suppress overflow checks (and thus panics). A wrapping
                    // add is equivalent to a non-wrapping add in this case
                    // because, due to how we computed `to_write` above, `pos +
                    // to_write` is less than or equal to `pos.len()`, which
                    // fits in a `usize` without wrapping.
                    self.pos = self.pos.wrapping_add(to_write);

                    Ok(())
                }
            }

            // Grab a mutable reference to our static buffer.
            //
            // Safety: alright. so. this can technically produce an aliasing
            // &mut in the case where we are panicking recursively. However, the
            // &mut taken in the earlier panic handler invocation is lost,
            // because the routine panicked and we'll never return to it. It's
            // not clear that a formalism like stacked borrows recognizes this,
            // but I'm pretty sure it's the best we can do.
            let buf = unsafe { &mut *core::ptr::addr_of_mut!(PANIC_BUFFER) };

            // Render the panic message!
            let mut writer = LimitedWriter { buf, pos: 0 };
            write!(writer, "{}", info).ok();

            // Pass the rendered portion of our buffer to the kernel.
            //
            // Safety: LimitedWriter takes pains to ensure that `pos` does not
            // pass `buf.len()`, so `get_unchecked` here is fine. We are using
            // `get_unchecked` to avoid the panic in `Index`, which the compiler
            // can often prove can't happen, but that's not guaranteed.
            sys_panic(unsafe { writer.buf.get_unchecked(..writer.pos) })
        }

    }
}

/// Utility function for sending an IPC with automatic retry if the server has
/// restarted.
///
/// This is intended for the common case where a server has either _not_
/// restarted, or is restarting infrequently. In that case, the appropriate
/// response is to use the dead code returned from `sys_send` to update our
/// record of the `TaskId`, and send the message again.
///
/// This function has no sleep, exponential backoff, or other such techniques
/// for avoiding using 100% of the CPU if a server is crashlooping rapidly. This
/// is because there's no single choice that's right for every application. If
/// you need something fancier, you'll have to implement it using your knowledge
/// of your application environment.
pub fn send_with_retry_on_death(
    tid_holder: &Cell<TaskId>,
    operation: u16,
    outgoing: &[u8],
    incoming: &mut [u8],
    leases: &mut [Lease<'_>],
) -> (ResponseCode, usize) {
    loop {
        let r = sys_send(
            tid_holder.get(),
            operation,
            outgoing,
            incoming,
            leases,
        );
        match r {
            Ok(rc_and_len) => break rc_and_len,
            Err(dead) => {
                tid_holder.set(
                    tid_holder.get().with_generation(dead.new_generation())
                );
            }
        }
    }
}

include!(concat!(env!("OUT_DIR"), "/hubris_abi_version.rs"));
