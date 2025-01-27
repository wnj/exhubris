use std::io::Read;

use serde::Serialize;

/// Incrementally constructs the `HUBRIS_IMAGE_ID` value used to
/// probabilistically distinguish builds.
#[derive(Clone, Default, Debug)]
pub struct BuildId {
    hasher: blake3::Hasher,
}

impl BuildId {
    /// Creates a new default `BuildId`.
    pub fn new() -> Self {
        Self {
            hasher: blake3::Hasher::new()
        }
    }

    /// Appends `data` to the internal hash.
    pub fn eat(&mut self, data: &[u8]) {
        self.hasher.update(data);
    }

    /// Serializes `something` to JSON and appends that to the internal hash.
    pub fn hash(&mut self, something: &impl Serialize) {
        serde_json::to_writer(&mut self.hasher, something).unwrap();
    }

    /// Generates 64 bits of ID from the internal hash.
    ///
    /// This destroys `self` so you don't accidentally feed it more data that
    /// won't be included in the result.
    pub fn finish(self) -> u64 {
        let mut bytes = [0; 8];
        self.hasher.finalize_xof().read_exact(&mut bytes).unwrap();
        u64::from_le_bytes(bytes)
    }
}
