pub struct Mmu;

impl Mmu {
    pub fn new() -> Mmu {
        Mmu
    }

    pub fn rb(&self, addr: u16) -> u8 {
        // TODO
        addr as u8
    }

    pub fn rw(&self, addr: u16) -> u16 {
        // TODO
        addr
    }

    pub fn wb(&mut self, addr: u16, val: u8) {
        // TODO
    }

    pub fn ww(&mut self, addr: u16, val: u16) {
        // TODO
    }
}
