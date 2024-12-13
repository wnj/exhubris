use std::io::Read;

use serde::Serialize;

#[derive(Clone, Default, Debug)]
pub struct BuildId {
    hasher: blake3::Hasher,
}

impl BuildId {
    pub fn new() -> Self {
        Self {
            hasher: blake3::Hasher::new()
        }
    }

    pub fn eat(&mut self, data: &[u8]) {
        self.hasher.update(data);
    }

    pub fn hash(&mut self, something: &impl Serialize) {
        serde_json::to_writer(&mut self.hasher, something).unwrap();
    }

    pub fn finish(&self) -> u64 {
        let mut bytes = [0; 8];
        self.hasher.finalize_xof().read_exact(&mut bytes).unwrap();
        u64::from_le_bytes(bytes)
    }
}
