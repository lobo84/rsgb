
pub struct Memory {
    data: [u8; 0xFFFF],
}

impl Memory {
    pub(crate) fn load(&mut self, data: &[u8]) {
        self.data[..data.len()].copy_from_slice(data);
    }
}

impl Default for Memory {
    fn default() -> Self {
        Self { data: [0; 0xFFFF] }
    }
}

impl Memory {
    pub fn read8(&self, addr: u16) -> u8 {
        return self.data[addr as usize];
    }
    pub fn read16(&self, addr: u16) -> u16 {
        let i = addr as usize;
        let hi = (self.data[i + 1] as u16) << 8;
        let lo = self.data[i] as u16;
        return hi | lo;
    }

    pub fn write8(&mut self, addr: u16, value: u8) {
        self.data[addr as usize] = value;
    }
    pub fn write16(&mut self, addr: u16, value: u16) {
        let [hi, lo] = value.to_be_bytes();
        self.data[addr as usize] = lo;
        self.data[addr as usize + 1] = hi;
    }
}

