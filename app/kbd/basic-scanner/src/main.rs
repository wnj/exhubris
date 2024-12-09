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
use drv_stm32l4_sys_api::{Stm32L4Sys as Sys, Port, Pull};
use idyll_runtime::NotificationHandler;
use userlib::{Message, ReplyFaultReason, TaskId};

const INTERVAL: u64 = 1; // millisecond

const QUEUE_DEPTH: usize = 16;

#[export_name = "main"]
fn main() -> ! {
    let sys = Sys::from(SLOTS.sys);

    for (port, pin) in config::ROWS {
        sys.set_pin_output(port, pin);
    }

    for (port, pin) in config::COLS {
        sys.set_pin_pull(port, pin, Some(Pull::Down));
        sys.set_pin_input(port, pin);
    }

    // Record the current time so we can manage our timer properly.
    let next_time = userlib::sys_get_timer().now + INTERVAL;
    userlib::sys_set_timer(Some(next_time), hubris_notifications::TIMER);

    // Event loop!
    let mut buffer = [MaybeUninit::uninit(); SCANNER_BUFFER_SIZE];
    let mut server = Server {
        next_time,
        scan_row: 0,
        keys_down: [[false; config::COLS.len()]; config::ROWS.len()],
        sys,
        queue: heapless::Deque::new(),
        keyboard: TaskId::gen0(config::KEYBOARD_TASK_INDEX),
    };
    loop {
        idyll_runtime::dispatch_or_event(
            &mut server,
            hubris_notifications::TIMER,
            &mut buffer,
        );
    }
}

struct Server {
    next_time: u64,
    scan_row: usize,
    keys_down: [[bool; config::COLS.len()]; config::ROWS.len()],

    sys: Sys,

    queue: heapless::Deque<KeyEvent, QUEUE_DEPTH>,

    keyboard: TaskId,
}

impl Scanner for Server {
    fn pop_event(
        &mut self,
        _: &Message<'_>,
    ) -> Result<Option<KeyEvent>, ReplyFaultReason> {
        Ok(self.queue.pop_front())
    }
}

impl NotificationHandler for Server {
    fn handle_notification(&mut self, bits: u32) {
        if bits & hubris_notifications::TIMER != 0 {
            // Advance key scanning, but only if time has really elapsed. (This
            // check is me being pedantic: it's possible for other tasks to post
            // our timer notification instead of the kernel doing it, so, I
            // check the time.)
            let now = userlib::sys_get_timer().now;
            if now >= self.next_time {
                // A real timer event!

                // Read column inputs
                let downs = &mut self.keys_down[self.scan_row];
                let mut poke_keyboard = false;
                for (i, (port, pin)) in config::COLS.into_iter().enumerate() {
                    let down_now = self.sys.is_pin_high(port, pin);
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
                self.sys.set_pin_low(config::ROWS[self.scan_row].0, config::ROWS[self.scan_row].1);
                self.scan_row = (self.scan_row + 1) % config::ROWS.len();
                self.sys.set_pin_high(config::ROWS[self.scan_row].0, config::ROWS[self.scan_row].1);

                // Bump our timer to the next deadline.
                self.next_time += INTERVAL;
                userlib::sys_set_timer(Some(self.next_time), hubris_notifications::TIMER);
            }
        }
    }
}

include!(concat!(env!("OUT_DIR"), "/generated_server.rs"));
include!(concat!(env!("OUT_DIR"), "/config.rs"));
