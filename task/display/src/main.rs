//! A task that operates as s SPI client.
//!
//! This task exists to test the SPI link.

#![no_std]
#![no_main]

use core::mem::MaybeUninit;

use drv_stm32xx_sys_api::{PeripheralName, Port, Stm32Sys as Sys};
use hubris_task_slots::SLOTS;
use idyll_runtime::NotificationHandler;
use stm32_metapac::timer::vals::Urs::COUNTERONLY;

#[export_name = "main"]
fn main() -> ! {
    // Create a client for the Sys driver and make our LED pin an output.
    let sys = Sys::from(SLOTS.sys);

    sys.enable_clock(PeripheralName::Tim17);

    // high speed timer for refreshing the 7-segment displays
    let tim = stm32_metapac::TIM17;

    //----
    // PA1 (L3) set to toggle output
    sys.set_pin_output(Port::A, 1);
    sys.set_pin_low(Port::A, 1);

    // By default the timer is receiving 64 MHz from the bus fabric. Divide that
    // by 64 to give a microsecond timebase.
    tim.psc().write(|w| w.set_psc(64 - 1)); // 1us

    // Let's wake up every 64 us.
    tim.arr().write(|w| w.set_arr(16));

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
        // w.set_opm(true);
        w.set_urs(COUNTERONLY);
        w.set_cen(true);
    });

    // Turn on our IRQ. It'll fire real soon now but we won't notice until we
    // recv.
    // sys_enable_irq_and_clear_pending(hubris_notifications::TIM_IRQ);
    //----

    // LED L12

    sys.set_pin_output(Port::A, 0);

    // 7-segment displays
    // CAn = high (off)
    sys.set_pin_output(Port::B, 6); // DP-1 (right most)
    sys.set_pin_high(Port::B, 6);
    sys.set_pin_output(Port::B, 7); // DP-2
    sys.set_pin_high(Port::B, 7);
    sys.set_pin_output(Port::B, 8); // DP-3
    sys.set_pin_high(Port::B, 8);
    sys.set_pin_output(Port::B, 9); // DP-4 (left most)
    sys.set_pin_high(Port::B, 9);

    // segments a..f, dp (off)
    sys.set_pin_output(Port::A, 4); // segment a
    sys.set_pin_high(Port::A, 4);
    sys.set_pin_output(Port::A, 5); // segment b
    sys.set_pin_high(Port::A, 5);
    sys.set_pin_output(Port::A, 6); // segment c
    sys.set_pin_high(Port::A, 6);
    sys.set_pin_output(Port::A, 7); // segment d
    sys.set_pin_high(Port::A, 7);
    sys.set_pin_output(Port::A, 8); // segment e
    sys.set_pin_high(Port::A, 8);
    sys.set_pin_output(Port::A, 9); // segment f
    sys.set_pin_high(Port::A, 9);
    sys.set_pin_output(Port::A, 10); // segment g
    sys.set_pin_high(Port::A, 10);
    sys.set_pin_output(Port::A, 11); // segment dp
    sys.set_pin_high(Port::A, 11);

    for p in 0..2 {
        sys.set_pin_low(Port::A, p);
    }

    for p in 4..12 {
        sys.set_pin_low(Port::A, p);
    }

    // let mut server = Server { tim };
    // let mut incoming = [MaybeUninit::uninit(); 4];

    // 4x 7-segment displays, colon L12
    let mut display = 0;

    loop {
        // idyll_runtime::dispatch_or_event(&mut server, hubris_notifications::TIM_IRQ, &mut incoming);

        userlib::sys_enable_irq_and_clear_pending(hubris_notifications::TIM_IRQ);
        userlib::sys_recv_notification(hubris_notifications::TIM_IRQ);
        sys.toggle_pin(Port::A, 1);
    }
    // // Record the current time so we can start our delay loop properly.
    // let mut next_send = userlib::sys_get_timer().now;

    // const INTERVAL: u64 = 500; // milliseconds

    // let mut display = 0;

    // loop {
    //     userlib::sys_set_timer(Some(next_send), hubris_notifications::TIMER_DISPLAY);

    //     // The proper thing to do, when waiting for a timer, is to sleep waiting
    //     // for notifications _and then check the time._ Otherwise other tasks
    //     // can wake you up by posting.
    //     loop {
    //         userlib::sys_recv_notification(hubris_notifications::TIMER_DISPLAY);
    //         let now = userlib::sys_get_timer().now;
    //         if now >= next_send {
    //             next_send += INTERVAL;
    //             break;
    //         }
    //     }

    //     sys.toggle_pin(Port::A, 0);

    //     if display == 0 {
    //         display = 6;
    //         sys.set_pin_high(Port::B, 6);
    //         sys.set_pin_high(Port::B, 7);
    //         sys.set_pin_high(Port::B, 8);
    //         sys.set_pin_high(Port::B, 9);
    //     }

    //     sys.set_pin_low(Port::B, display);

    //     display += 1;

    //     if display > 9 {
    //         display = 0;
    //     }
    // }
}

// include!(concat!(env!("OUT_DIR"), "/generated_server.rs"));
// include!(concat!(env!("OUT_DIR"), "/config.rs"));

// struct Server {
//     tim: stm32_metapac::timer::TimGp16,
// }

// impl NotificationHandler for Server {
//     fn handle_notification(&mut self, bits: u32) {
//         // let sys = Sys::from(SLOTS.sys);

//         if bits & hubris_notifications::TIM_IRQ != 0 {
//             // Advance key scanning, but only if time has really elapsed. (This
//             // check is me being pedantic: it's possible for other tasks to post
//             // our timer notification instead of the kernel doing it, so, I
//             // check the time.)
//             if self.tim.sr().read().uif() {
//                 // A real timer event! Clear the interrupt condition at the
//                 // timer...
//                 self.tim.sr().write(|w| {
//                     // write 1 to preserve
//                     w.0 = !0;
//                     // ...except the update flag
//                     w.set_uif(false);
//                 });

//                 // ...and re-enable in the kernel.
//                 sys_enable_irq_and_clear_pending(hubris_notifications::TIM_IRQ);

//                 // sys.toggle_pin(Port::C, 14);

//                 // Read column inputs

//                 // Turn off this row and turn on the next.

//                 // Restart our timer. We do this _after_ turning on the GPIO
//                 // above so that it acts as a propagation delay before we read.
//                 self.tim.cr1().modify(|w| w.set_cen(true));
//             }
//         }
//     }
// }
