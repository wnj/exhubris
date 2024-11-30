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
use drv_stm32g0_sys_api::{Stm32G0Sys as Sys, PeripheralName};

/// Counter for viewing in the debugger.
#[no_mangle]
static mut CHARS_SENT: u32 = 0;

#[export_name = "main"]
fn main() -> ! {
    let sys = Sys::from(SLOTS.sys);

    // Turn on USART2
    sys.enable_clock(PeripheralName::Usart2);

    // Initialize UART
    let uart = stm32_metapac::USART2;

    uart.brr().write(|w| {
        w.set_brr((config::UART_CLOCK_HZ / config::BAUD_RATE) as u16);
    });

    uart.cr1().write(|w| {
        w.set_rxneie(true);
        w.set_re(true);
        w.set_te(true);
        w.set_ue(true);
    });

    // Set pin A2+A3 to USART2
    for (port, pin, af) in config::PINS {
        sys.set_pin_alternate_mode(port, pin, af);
    }

    loop {
        // Enable the UART's IRQ output to reach our notification bit.
        userlib::sys_enable_irq(hubris_notifications::USART_IRQ);
        // Block waiting for that notification bit to be set.
        userlib::sys_recv_notification(hubris_notifications::USART_IRQ);

        // Transfer all pending characters.
        while uart.isr().read().rxne() {
            let byte = uart.rdr().read().dr() & 0xFF;
            uart.tdr().write(|w| w.set_dr(byte));
            unsafe {
                CHARS_SENT = CHARS_SENT.wrapping_add(1);
            }
        }
    }
}

include!(concat!(env!("OUT_DIR"), "/task_config.rs"));
