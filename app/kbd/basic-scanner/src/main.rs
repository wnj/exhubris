//! Basic GPIO keyboard matrix scanner.
//!
//! This scans a rectangular matrix by setting one "row" pin high at a time,
//! waiting for a bit, and then reading the state of all "column" pins. Any
//! changes are queued up for retrieval by another task.
//!
//! Currently, this uses the kernel general-purpose timer, which means that (by
//! default) it's restricted to millisecond precision. This limits the maximum
//! scan rate to `1000 / num_rows` Hz, which is slow by modern standards. This
//! could be fixed by enlisting a hardware timer to generate faster interrupts,
//! or by using DMA to perform the scan, both of which are currently out of
//! scope for this _basic_ scanner implementation.

#![no_std]
#![no_main]

use core::mem::MaybeUninit;

use hubris_task_slots::SLOTS;
use drv_stm32l4_sys_api::{Stm32L4Sys as Sys, Port, Pull, PeripheralName};
use idyll_runtime::{NotificationHandler, Meta};
use userlib::{sys_enable_irq_and_clear_pending, ReplyFaultReason, TaskId};

const INTERVAL: u32 = 62 - 15; // microseconds; fudge factor adjusts for code
                               // execution time

const QUEUE_DEPTH: usize = 16;

#[export_name = "main"]
fn main() -> ! {
    let sys = Sys::from(SLOTS.sys);

    sys.enable_clock(PeripheralName::Tim2);

    for (port, pin) in config::ROWS {
        sys.set_pin_output(port, pin);
    }

    for (port, pin) in config::COLS {
        sys.set_pin_pull(port, pin, Some(Pull::Down));
        sys.set_pin_input(port, pin);
    }

    // Alright, we're done using SYS for performance reasons, start poking the
    // GPIO directly.
    for (port, pin) in config::ROWS {
        set_pin_low(port, pin);
    }

    let tim = stm32_metapac::TIM2;
    // By default the timer is receiving 80 MHz from the bus fabric. Divide that
    // by 80 to give a microsecond timebase.
    tim.psc().write(|w| w.set_psc(80 - 1));
    // Let's wake up every INTERVAL us.
    tim.arr().write(|w| w.set_arr(INTERVAL - 1));
    // Trigger an update event so those registers get applied.
    tim.egr().write(|w| w.set_ug(true));
    // Clear any status bits that just got set by that. All writable bits in
    // this register are write-zero-to-clear.
    tim.sr().write(|w| w.0 = 0);
    // Enable interrupt on timer overflow (update).
    tim.dier().write(|w| w.set_uie(true));
    // Start the timer!
    tim.cr1().write(|w| {
        // One-pulse mode causes the timer to interrupt once and turn itself
        // off, which gets us our "at least" sleep behavior more effectively
        // than a cyclic repeating interrupt would ... but it does mean we have
        // to remember to turn it back on!
        w.set_opm(true);
        w.set_cen(true);
    });

    // Turn on our IRQ. It'll fire real soon now but we won't notice until we
    // recv.
    sys_enable_irq_and_clear_pending(hubris_notifications::TIM_IRQ);

    // Event loop!
    let mut buffer = [MaybeUninit::uninit(); SCANNER_BUFFER_SIZE];
    let mut server = Server {
        tim,
        scan_row: 0,
        keys_down: [[false; config::COLS.len()]; config::ROWS.len()],
        queue: heapless::Deque::new(),
        keyboard: TaskId::gen0(config::KEYBOARD_TASK_INDEX),
    };
    loop {
        idyll_runtime::dispatch_or_event(
            &mut server,
            hubris_notifications::TIM_IRQ,
            &mut buffer,
        );
    }
}

struct Server {
    tim: stm32_metapac::timer::TimGp32,
    scan_row: usize,
    keys_down: [[bool; config::COLS.len()]; config::ROWS.len()],

    queue: heapless::Deque<KeyEvent, QUEUE_DEPTH>,

    keyboard: TaskId,
}

impl Scanner for Server {
    fn pop_event(
        &mut self,
        _: Meta,
    ) -> Result<Option<KeyEvent>, ReplyFaultReason> {
        Ok(self.queue.pop_front())
    }
}

impl NotificationHandler for Server {
    fn handle_notification(&mut self, bits: u32) {
        if bits & hubris_notifications::TIM_IRQ != 0 {
            // Advance key scanning, but only if time has really elapsed. (This
            // check is me being pedantic: it's possible for other tasks to post
            // our timer notification instead of the kernel doing it, so, I
            // check the time.)
            if self.tim.sr().read().uif() {
                // A real timer event! Clear the interrupt condition at the
                // timer...
                self.tim.sr().write(|w| {
                    // write 1 to preserve
                    w.0 = !0;
                    // ...except the update flag
                    w.set_uif(false);
                });
                // ...and re-enable in the kernel.
                sys_enable_irq_and_clear_pending(hubris_notifications::TIM_IRQ);

                // Read column inputs
                let downs = &mut self.keys_down[self.scan_row];
                let mut poke_keyboard = false;
                for (i, (port, pin)) in config::COLS.into_iter().enumerate() {
                    let gpio = get_port(port);

                    let down_now = gpio.idr().read().0 & (1 << pin) != 0;
                    let change = match (downs[i], down_now) {
                        (false, true) => Some(KeyState::Down),
                        (true, false) => Some(KeyState::Up),
                        _ => None,
                    };
                    if let Some(state) = change {
                        self.queue.push_back(KeyEvent { state, row: self.scan_row as u8, col: i as u8 }).ok();
                        poke_keyboard = true;
                    }
                    downs[i] = down_now;
                }
                if poke_keyboard {
                    loop {
                        match userlib::sys_post(self.keyboard, config::KEYBOARD_NOTIFICATION_MASK) {
                            Ok(()) => break,
                            Err(dead) => {
                                // Update and try again. Since we're higher priority, this
                                // shouldn't loop more than once.
                                self.keyboard =
                                    self.keyboard.with_generation(dead.new_generation());
                            }
                        }
                    }
                }

                // Turn off this row and turn on the next.
                set_pin_low(config::ROWS[self.scan_row].0, config::ROWS[self.scan_row].1);
                self.scan_row = (self.scan_row + 1) % config::ROWS.len();
                set_pin_high(config::ROWS[self.scan_row].0, config::ROWS[self.scan_row].1);

                // Restart our timer. We do this _after_ turning on the GPIO
                // above so that it acts as a propagation delay before we read.
                self.tim.cr1().modify(|w| w.set_cen(true));
            }
        }
    }
}

fn set_pin_low(port: Port, pin: u8) {
    let gpio = get_port(port);
    let pin = pin as usize & 0xF;
    gpio.bsrr().write(|w| w.set_br(pin, true));
}

fn set_pin_high(port: Port, pin: u8) {
    let gpio = get_port(port);
    let pin = pin as usize & 0xF;
    gpio.bsrr().write(|w| w.set_bs(pin, true));
}

fn get_port(port: Port) -> stm32_metapac::gpio::Gpio {
    match port {
        Port::A => stm32_metapac::GPIOA,
        Port::B => stm32_metapac::GPIOB,
        Port::C => stm32_metapac::GPIOC,
        Port::D => stm32_metapac::GPIOD,
        Port::H => stm32_metapac::GPIOH,
    }
}

include!(concat!(env!("OUT_DIR"), "/generated_server.rs"));
include!(concat!(env!("OUT_DIR"), "/config.rs"));
