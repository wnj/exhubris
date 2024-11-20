//! Exposes the number of tasks in the current Hubris application as a
//! compile-time constant, for sizing tables and the like.

#![no_std]

/// Number of tasks in the current application.
pub const NUM_TASKS: usize = include!(concat!(env!("OUT_DIR"), "/task_count.txt"));
