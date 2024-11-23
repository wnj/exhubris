#![no_std]

use core::cell::Cell;
use userlib::{TaskId, send_with_retry_on_death};

/// A handle to the Sys task.
pub struct Sys(Cell<TaskId>);

impl Sys {
    pub fn enable_clock(&self, peripheral: PeripheralName) {
        send_with_retry_on_death(
            &self.0,
            Operation::EnableClock as u16,
            &(peripheral as u16).to_le_bytes(),
            &mut [],
            &mut [],
        );
    }

    pub fn set_pin_output(&self, port: Port, pin: u8) {
        let message = (port as u16) << 4
            | u16::from(pin & 0b1111);
        send_with_retry_on_death(
            &self.0,
            Operation::SetPinOutput as u16,
            &message.to_le_bytes(),
            &mut [],
            &mut [],
        );
    }

    pub fn set_pin_alternate_mode(&self, port: Port, pin: u8, af: u8) {
        let message = u16::from(af & 0b111) << 8
            | (port as u16) << 4
            | u16::from(pin & 0b1111);
        send_with_retry_on_death(
            &self.0,
            Operation::SetPinAlternate as u16,
            &message.to_le_bytes(),
            &mut [],
            &mut [],
        );
    }

    pub fn toggle_pin(&self, port: Port, pin: u8) {
        let message = (port as u16) << 4
            | u16::from(pin & 0b1111);
        send_with_retry_on_death(
            &self.0,
            Operation::TogglePin as u16,
            &message.to_le_bytes(),
            &mut [],
            &mut [],
        );
    }
}

enum Operation {
    SetPinOutput = 0,
    TogglePin = 3,
    SetPinAlternate = 4,
    EnableClock = 5,
}

impl From<TaskId> for Sys {
    fn from(tid: TaskId) -> Self {
        Self(Cell::new(tid))
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum PeripheralName {
    Usart2 = 0b10_10001,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Port { A = 0, B, C, D, E, F }
