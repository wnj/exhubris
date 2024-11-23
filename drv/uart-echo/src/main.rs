//! Aggressively minimal UART echo demo for the stm32g0 nucleo board.
//!
//! This brings up USART2 on PA2/3 at 9600 baud, assuming a central clock
//! frequency of 16 MHz. It then echoes whatever it receives, forever.
//!
//! The raw sends to sys here are pretty gross.

#![no_std]
#![no_main]

use userlib as _;
use hubris_task_slots::SLOTS;

#[no_mangle]
static mut CHARS_SENT: u32 = 0;

#[export_name = "main"]
fn main() -> ! {
    let sys = SLOTS.sys;

    // Turn on USART2
    userlib::sys_send(
        sys,
        5,
        &(0b10_10001_u16).to_le_bytes(),
        &mut [],
        &mut [],
    ).ok();

    // Initialize UART
    let uart = stm32_metapac::USART2;

    uart.brr().write(|w| {
        w.set_brr((16_000_000_u32 / 9600) as u16);
    });

    uart.cr1().write(|w| {
        w.set_rxneie(true);
        w.set_re(true);
        w.set_te(true);
        w.set_ue(true);
    });

    // Set pin A2+A3 to USART2
    for pin in [2, 3] {
        userlib::sys_send(
            sys,
            4,
            &(0b0001_0000_0000_u16 | pin).to_le_bytes(),
            &mut [],
            &mut [],
        ).ok();
    }

    loop {
        userlib::sys_enable_irq(hubris_notifications::USART_2_IRQ);
        userlib::sys_recv_notification(hubris_notifications::USART_2_IRQ);

        while uart.isr().read().rxne() {
            let byte = uart.rdr().read().dr() & 0xFF;
            uart.tdr().write(|w| w.set_dr(byte));
            unsafe {
                CHARS_SENT = CHARS_SENT.wrapping_add(1);
            }
        }
    }
}
