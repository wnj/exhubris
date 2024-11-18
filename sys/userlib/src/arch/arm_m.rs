use core::arch::global_asm;
use core::mem::MaybeUninit;
use crate::{Lease, AbiLease, TaskId, Sysnum, TaskDeath, ResponseCode, RecvMessage};

pub(crate) fn idle() {
    cortex_m::asm::wfi();
}

#[repr(C)]
struct SendArgs {
    target_and_operation: u32,
    outgoing_base: *const u8,
    outgoing_len: usize,
    incoming_base: *mut u8,
    incoming_len: usize,
    lease_base: *const AbiLease,
    lease_count: usize,
}

#[inline(always)]
pub fn sys_send(
    target: TaskId,
    operation: u16,
    outgoing: &[u8],
    incoming: &mut [u8],
    leases: &mut [Lease<'_>],
) -> Result<(ResponseCode, usize), TaskDeath> {
    let target_and_operation = u32::from(target.0) << 16
            | u32::from(operation);
    let ret64 = unsafe {
        sys_send_stub2(
            target_and_operation,
            outgoing.as_ptr(),
            outgoing.len(),
            incoming.as_mut_ptr(),
            incoming.len(),
            leases.as_mut_ptr().cast(),
            leases.len(),
        )
    };
    let retval = ResponseCode(ret64 as u32);
    if let Ok(e) = TaskDeath::try_from(retval) {
        Err(e)
    } else {
        Ok((retval, (ret64 >> 32) as usize))
    }
}

global_asm!("
.section .text.sys_send_stub
.globl sys_send_stub
.type sys_send_stub,function
sys_send_stub:
    .cfi_startproc

    @ Stash the register values we're about to destroy.
    push {{r4-r7, lr}}
    .cfi_adjust_cfa_offset 20
    .cfi_offset r4, -20
    .cfi_offset r5, -16
    .cfi_offset r6, -12
    .cfi_offset r7, -8
    .cfi_offset lr, -4

    mov r4, r8
    mov r5, r9
    mov r6, r10
    mov r7, r11
    push {{r4-r7}}
    .cfi_adjust_cfa_offset 16
    .cfi_offset r4, -36
    .cfi_offset r5, -32
    .cfi_offset r6, -28
    .cfi_offset r7, -24
    
    @ Materialize the sysnum constant.
    eors r4, r4
    adds r4, #{sysnum}
    mov r11, r4

    @ Load arguments from the parameter struct.
    ldm r0!, {{r4-r7}}
    ldm r0, {{r0-r2}}
    mov r8, r0
    mov r9, r1
    mov r10, r2

    svc #0

    @ Put the results into the function return position.
    mov r0, r4
    mov r1, r5
    @ Restore the registers.
    pop {{r4-r7}}
    .cfi_adjust_cfa_offset -16
    mov r8, r4
    mov r9, r5
    mov r10, r6
    mov r11, r7
    pop {{r4-r7, pc}}

    .cfi_endproc
",
    sysnum = const Sysnum::Send as u32,
);

global_asm!("
.section .text.sys_send_stub2
.globl sys_send_stub2
.type sys_send_stub2,function
sys_send_stub2:
    .cfi_startproc

    @ We get the first four arguments in r0-r3, and the next three on the stack.
    @ We need one additional working register for the syscall number, for a total
    @ of eight.

    @ Stash the register values we're about to destroy.
    push {{r4-r7}}
    .cfi_adjust_cfa_offset 16
    .cfi_offset r4, -16
    .cfi_offset r5, -12
    .cfi_offset r6, -8
    .cfi_offset r7, -4

    mov r4, r8
    mov r5, r9
    mov r6, r10
    mov r7, r11
    push {{r4-r7}}
    .cfi_adjust_cfa_offset 16
    .cfi_offset r4, -32
    .cfi_offset r5, -28
    .cfi_offset r6, -24
    .cfi_offset r7, -20
    
    @ Materialize the sysnum constant. For Thumb Reasons this has to go through
    @ r4 on the way, so we do this while we still have a bunch of temp registers
    @ free.
    movs r4, #{sysnum}
    mov r11, r4

    @ Load the three operands from the stack. We've pushed eight words onto
    @ the stack, so we need to address _past_ that.
    add r4, sp, #(8 * 4)
    ldm r4, {{r4-r6}}
    mov r8, r4
    mov r9, r5
    mov r10, r6

    @ Copy up register operands.
    mov r4, r0
    mov r5, r1
    mov r6, r2
    mov r7, r3

    svc #0

    @ Put the results into the function return position.
    mov r0, r4
    mov r1, r5
    @ Restore the registers.
    pop {{r4-r7}}
    .cfi_adjust_cfa_offset -16
    mov r8, r4
    mov r9, r5
    mov r10, r6
    mov r11, r7
    pop {{r4-r7}}
    bx lr

    .cfi_endproc
",
    sysnum = const Sysnum::Send as u32,
);

#[repr(C)]
struct AbiRecvMessage {
    sender: u32,
    operation_or_notification: u32,
    sent_length: usize,
    reply_capacity: usize,
    lease_count: usize,
}

#[inline(always)]
pub fn sys_recv(
    incoming: &mut [u8],
    notification_mask: u32,
    from: Option<TaskId>,
) -> Result<RecvMessage, TaskDeath> {
    let mut out = MaybeUninit::<AbiRecvMessage>::uninit();
    let retval = unsafe {
        sys_recv_stub(
            incoming.as_mut_ptr(),
            incoming.len(),
            notification_mask,
            from.map(|tid| 0x8000_0000 | u32::from(tid.0)).unwrap_or(0),
            out.as_mut_ptr(),
        )
    };

    // Safety: our asm stub is responsible for fully initializing this struct
    // whether we succeed or fail.
    let retval = ResponseCode(retval);
    if let Ok(e) = TaskDeath::try_from(retval) {
        Err(e)
    } else {
        let rm = unsafe { out.assume_init() };
        Ok(RecvMessage {
            sender: TaskId(rm.sender as u16),
            operation_or_notification: rm.operation_or_notification,
            sent_length: rm.sent_length,
            reply_capacity: rm.reply_capacity,
            lease_count: rm.lease_count,
        })
    }
}

#[inline(always)]
pub fn sys_recv_open(
    incoming: &mut [u8],
    notification_mask: u32,
) -> RecvMessage {
    let mut out = MaybeUninit::<AbiRecvMessage>::uninit();
    unsafe {
        sys_recv_stub(
            incoming.as_mut_ptr(),
            incoming.len(),
            notification_mask,
            0,
            out.as_mut_ptr(),
        );
    }

    // Safety: our asm stub is responsible for fully initializing this struct
    // whether we succeed or fail.
    let rm = unsafe { out.assume_init() };
    RecvMessage {
        sender: TaskId(rm.sender as u16),
        operation_or_notification: rm.operation_or_notification,
        sent_length: rm.sent_length,
        reply_capacity: rm.reply_capacity,
        lease_count: rm.lease_count,
    }
}

global_asm!("
.section .text.sys_recv_stub
.globl sys_recv_stub
.type sys_recv_stub,function
sys_recv_stub:
    .cfi_startproc

    @ Stash the register values we're about to destroy.
    push {{r4-r7, lr}}
    .cfi_adjust_cfa_offset 20
    .cfi_offset r4, -20
    .cfi_offset r5, -16
    .cfi_offset r6, -12
    .cfi_offset r7, -8
    .cfi_offset lr, -4

    mov r4, r8
    mov r5, r9
    mov r6, r10
    mov r7, r11
    push {{r4-r7}}
    .cfi_adjust_cfa_offset 16
    .cfi_offset r4, -36
    .cfi_offset r5, -32
    .cfi_offset r6, -28
    .cfi_offset r7, -24

    @ Materialize the sysnum constant.
    eors r4, r4
    adds r4, #{sysnum}
    mov r11, r4

    @ Move arguments 0-3 into the correct registers.
    mov r4, r0
    mov r5, r1
    mov r6, r2
    mov r7, r3

    @ Collect the final argument from the stack. This isn't an argument to the
    @ kernel, it's our out-buffer-pointer. We could technically do this _after_
    @ the syscall.
    ldr r3, [sp, #(9 * 4)]
    
    svc #0

    @ Status flag into return position.
    mov r0, r4
    @ Write the rest through our out-buffer pointer.
    stm r3!, {{r5-r7}}
    mov r5, r8
    mov r6, r9
    stm r3!, {{r5-r6}}

    @ Restore the registers.
    pop {{r4-r7}}
    .cfi_adjust_cfa_offset -16
    mov r8, r4
    mov r9, r5
    mov r10, r6
    mov r11, r7
    pop {{r4-r7, pc}}

    .cfi_endproc
",
    sysnum = const Sysnum::Recv as u32,
);

#[inline(always)]
pub fn sys_reply(
    sender: TaskId,
    code: ResponseCode,
    message: &[u8],
) {
    unsafe {
        sys_reply_stub(
            sender.0 as u32,
            code.0,
            message.as_ptr(),
            message.len(),
        )
    }
}

global_asm!("
.section .text.sys_reply_stub
.globl sys_reply_stub
.type sys_reply_stub,function
sys_reply_stub:
    .cfi_startproc

    @ Stash the register values we're about to destroy.
    push {{r4-r7, lr}}
    .cfi_adjust_cfa_offset 20
    .cfi_offset r4, -20
    .cfi_offset r5, -16
    .cfi_offset r6, -12
    .cfi_offset r7, -8
    .cfi_offset lr, -4

    mov r4, r11
    push {{r4}}
    .cfi_adjust_cfa_offset 4
    .cfi_offset r11, -24

    @ Materialize the sysnum constant.
    eors r4, r4
    adds r4, #{sysnum}
    mov r11, r4

    @ Move arguments 0-3 into the correct registers.
    mov r4, r0
    mov r5, r1
    mov r6, r2
    mov r7, r3

    svc #0

    @ Restore the registers.
    pop {{r4}}
    .cfi_adjust_cfa_offset -4
    mov r11, r4
    pop {{r4-r7, pc}}

    .cfi_endproc
",
    sysnum = const Sysnum::Reply as u32,
);

pub fn sys_panic(msg: &[u8]) -> ! {
    unsafe {
        sys_panic_stub(msg.as_ptr(), msg.len())
    }
}

cfg_if::cfg_if! {
    if #[cfg(hubris_target = "thumbv7em-none-eabihf")] {
        global_asm!("
        .section .text.sys_panic_stub
        .globl sys_panic_stub
        .type sys_panic_stub,function
        sys_panic_stub:
            .cfi_startproc
        
            @ Stash the register values we're about to destroy, to make reconstructing the
            @ task's state at panic easier. (Since the task won't resume, these will never
            @ get popped.)
            push {{r4, r5, r11, lr}}
            .cfi_adjust_cfa_offset 16
            .cfi_offset r4, -16
            .cfi_offset r5, -12
            .cfi_offset r11, -8
            .cfi_offset lr, -4
        
            @ Move arguments into place.
            mov r4, r0
            mov r5, r1
            mov r11, {sysnum}
        
            svc #0
            udf 0xad
            .cfi_endproc
        ",
            sysnum = const Sysnum::Panic as u32,
        );
    } else if #[cfg(hubris_target = "thumbv6m-none-eabi")] {
        global_asm!("
        .section .text.sys_panic_stub
        .globl sys_panic_stub
        .type sys_panic_stub,function
        sys_panic_stub:
            .cfi_startproc
        
            @ Stash the register values we're about to destroy, to make reconstructing the
            @ task's state at panic easier. (Since the task won't resume, these will never
            @ get popped.)
            push {{r4, r5, lr}}
            .cfi_adjust_cfa_offset 16
            .cfi_offset r4, -16
            .cfi_offset r5, -12
            .cfi_offset r11, -8
            .cfi_offset lr, -4
            mov r4, r11
            .cfi_register r11, r4
            push {{r4}}
            .cfi_offset r11, -20
            .cfi_adjust_cfa_offset 4
       
            @ Materialize the sysnum constant.
            eors r4, r4
            adds r4, #{sysnum}
            mov r11, r4
            @ Move arguments into place.
            mov r4, r0
            mov r5, r1
        
            svc #0
            udf 0xad
            .cfi_endproc
        ",
            sysnum = const Sysnum::Panic as u32,
        );
    } else {
        compile_error!("unrecognized ARM target triple");
    }
}

extern "C" {
    /// # Safety
    ///
    /// The pointers in the SendArgs struct must be valid for read (outgoing,
    /// lease table) or write (incoming).
    fn sys_send_stub(_msg: &mut SendArgs) -> u64;

    fn sys_send_stub2(
        target_and_operation: u32,
        outgoing_base: *const u8,
        outgoing_len: usize,
        incoming_base: *mut u8,
        incoming_len: usize,
        lease_base: *const AbiLease,
        lease_count: usize,
    ) -> u64;

    fn sys_recv_stub(
        incoming_base: *mut u8,
        incoming_len: usize,
        notification_mask: u32,
        sender_bits: u32,
        out: *mut AbiRecvMessage,
    ) -> u32;

    fn sys_reply_stub(
        sender: u32,
        code: u32,
        outgoing_base: *const u8,
        outgoing_len: usize,
    );

    /// # Safety
    ///
    /// The pointer/length passed to this function must be valid.
    ///
    /// Well, strictly speaking, they don't, at least not from Rust's
    /// perspective. They will not be dereferenced in this lifetime. They're
    /// just sent to the kernel. But, it's a good idea to use valid pointers.
    fn sys_panic_stub(_msg: *const u8, _len: usize) -> !;

    pub fn _start() -> !;
}

extern "Rust" {
    fn main() -> !;
}

cfg_if::cfg_if! {
    if #[cfg(hubris_target = "thumbv6m-none-eabi")] {
        global_asm!("
        .section .text.start
        .globl _start
        _start:
            @ Copy data initialization image into data section.
            @ Note: this assumes that both source and destination are 32-bit
            @ aligned and padded to 4-byte boundary.

            ldr r0, =__edata            @ upper bound of output
            ldr r1, =__sidata           @ start of source image
            ldr r2, =__sdata            @ start of output

            b 1f                        @ jump to loop tail to detect empty .data

        2:  ldm r1!, {{r3}}             @ read and advance source
            stm r2!, {{r3}}             @ write and advance dest

        1:  cmp r2, r0                  @ done yet?
            bne 2b                      @ if not, repeat.
        
            @ Zero BSS section.
        
            ldr r0, =__ebss             @ upper bound of zeroed region
            ldr r1, =__sbss             @ lower bound
            movs r2, #0                 @ the all-important constant

            b 1f                        @ handle empty .bss

        2:  stm r1!, {{r2}}             @ zero one word and increment pointer
        1:  cmp r1, r0                  @ done yet?
            bne 2b                      @ no? continue.
        
            @ Now, to the user entry point. We call it in case it
            @ returns. (It's not supposed to.) We reference it through
            @ a sym operand because it's a Rust func and may be mangled.
            bl {main}
        
            @ Should main return... kill it.
            udf 0xad
            ",
            main = sym main,
        );
    } else if #[cfg(hubris_target = "thumbv7em-none-eabihf")] {
        global_asm!("
        .section .text.start
        .globl _start
        _start:
            @ Copy data initialization image into data section.
            @ Note: this assumes that both source and destination are 32-bit
            @ aligned and padded to 4-byte boundary.
        
            movw r0, #:lower16:__edata  @ upper bound in r0
            movt r0, #:upper16:__edata

            movw r1, #:lower16:__sidata @ source in r1
            movt r1, #:upper16:__sidata

            movw r2, #:lower16:__sdata  @ dest in r2
            movt r2, #:upper16:__sdata
        
            b 1f                        @ check for zero-sized data
        
        2:  ldr r3, [r1], #4            @ read and advance source
            str r3, [r2], #4            @ write and advance dest
        
        1:  cmp r2, r0                  @ has dest reached the upper bound?
            bne 2b                      @ if not, repeat
        
            @ Zero BSS section.
        
            movw r0, #:lower16:__ebss   @ upper bound in r0
            movt r0, #:upper16:__ebss
        
            movw r1, #:lower16:__sbss   @ base in r1
            movt r1, #:upper16:__sbss
        
            movs r2, #0                 @ materialize a zero
        
            b 1f                        @ check for zero-sized BSS
        
        2:  str r2, [r1], #4            @ zero one word and advance
        
        1:  cmp r1, r0                  @ has base reached bound?
            bne 2b                      @ if not, repeat
        
            @ Now, to the user entry point. We call it in case it
            @ returns. (It's not supposed to.) We reference it through
            @ a sym operand because it's a Rust func and may be mangled.
            bl {main}
        
            @ Should main return... kill it.
            udf 0xad
            ",
            main = sym main,
        );
    } else {
        compile_error!("unrecognized target for start code");
    }
}
