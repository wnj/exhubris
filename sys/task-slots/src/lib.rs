//! Exposes the current task's binding of logical task names to application
//! indices.

#![no_std]

include!(concat!(env!("OUT_DIR"), "/task_slots.rs"));
