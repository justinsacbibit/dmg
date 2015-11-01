// TODO: Remove allows
#[allow(dead_code)]
#[allow(unused_variables)]
pub struct Mmu {
    in_bios: bool,
    bios: [u8; 256],
    rom: Vec<u8>,
    wram: [u8; 8192],
    eram: [u8; 8192],
    zram: [u8; 127],
    ie_: u8, if_: u8,
}

// TODO: Remove allows
#[allow(dead_code)]
#[allow(unused_variables)]
impl Mmu {
    pub fn new() -> Mmu {
        Mmu {
            in_bios: true,
            bios: [
                0x31, 0xFE, 0xFF, 0xAF, 0x21, 0xFF, 0x9F, 0x32, 0xCB, 0x7C, 0x20, 0xFB, 0x21, 0x26, 0xFF, 0x0E,
                0x11, 0x3E, 0x80, 0x32, 0xE2, 0x0C, 0x3E, 0xF3, 0xE2, 0x32, 0x3E, 0x77, 0x77, 0x3E, 0xFC, 0xE0,
                0x47, 0x11, 0x04, 0x01, 0x21, 0x10, 0x80, 0x1A, 0xCD, 0x95, 0x00, 0xCD, 0x96, 0x00, 0x13, 0x7B,
                0xFE, 0x34, 0x20, 0xF3, 0x11, 0xD8, 0x00, 0x06, 0x08, 0x1A, 0x13, 0x22, 0x23, 0x05, 0x20, 0xF9,
                0x3E, 0x19, 0xEA, 0x10, 0x99, 0x21, 0x2F, 0x99, 0x0E, 0x0C, 0x3D, 0x28, 0x08, 0x32, 0x0D, 0x20,
                0xF9, 0x2E, 0x0F, 0x18, 0xF3, 0x67, 0x3E, 0x64, 0x57, 0xE0, 0x42, 0x3E, 0x91, 0xE0, 0x40, 0x04,
                0x1E, 0x02, 0x0E, 0x0C, 0xF0, 0x44, 0xFE, 0x90, 0x20, 0xFA, 0x0D, 0x20, 0xF7, 0x1D, 0x20, 0xF2,
                0x0E, 0x13, 0x24, 0x7C, 0x1E, 0x83, 0xFE, 0x62, 0x28, 0x06, 0x1E, 0xC1, 0xFE, 0x64, 0x20, 0x06,
                0x7B, 0xE2, 0x0C, 0x3E, 0x87, 0xF2, 0xF0, 0x42, 0x90, 0xE0, 0x42, 0x15, 0x20, 0xD2, 0x05, 0x20,
                0x4F, 0x16, 0x20, 0x18, 0xCB, 0x4F, 0x06, 0x04, 0xC5, 0xCB, 0x11, 0x17, 0xC1, 0xCB, 0x11, 0x17,
                0x05, 0x20, 0xF5, 0x22, 0x23, 0x22, 0x23, 0xC9, 0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B,
                0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D, 0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E,
                0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99, 0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC,
                0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E, 0x3c, 0x42, 0xB9, 0xA5, 0xB9, 0xA5, 0x42, 0x4C,
                0x21, 0x04, 0x01, 0x11, 0xA8, 0x00, 0x1A, 0x13, 0xBE, 0x20, 0xFE, 0x23, 0x7D, 0xFE, 0x34, 0x20,
                0xF5, 0x06, 0x19, 0x78, 0x86, 0x23, 0x05, 0x20, 0xFB, 0x86, 0x20, 0xFE, 0x3E, 0x01, 0xE0, 0x50
                    ],
            rom: Vec::new(),
            wram: [0; 8192],
            eram: [0; 8192],
            zram: [0; 127],
            ie_: 0, if_: 0,
        }
    }

    pub fn reset(&mut self) {
        self.in_bios = true;
        self.ie_ = 0;
        self.if_ = 0;
        self.wram = [0; 8192];
        self.eram = [0; 8192];
        self.zram = [0; 127];
    }

    pub fn load(&mut self, rom: Vec<u8>) {
        self.rom = rom;
    }

    pub fn rb(&self, addr: u16) -> u8 {
        match addr & 0xF000 {
            // ROM0 / BIOS(256b)
            0x0000 | 0x1000 | 0x2000 | 0x3000 =>
                if self.in_bios && addr < 0x0100 {
                    self.bios[addr as usize]
                } else {
                    self.rom[addr as usize]
                },

            // ROM1 (unbanked) (16k)
            0x4000 | 0x5000 | 0x6000 | 0x7000 => self.rom[addr as usize],

            // Graphics: VRAM (8k)
            0x8000 | 0x9000 => 0x00, // TODO: Hook up to GPU

            // External RAM (8k)
            0xA000 | 0xB000 => self.eram[(addr & 0x1FFF) as usize],

            // Working RAM (8k)
            0xC000 | 0xD000 | 0xE000 => self.wram[(addr & 0x1FFF) as usize],

            // Working RAM shadow, I/O, Zero-page RAM
            0xF000 => {
                match addr & 0x0F00 {
                    // Working RAM shadow
                    0x000 | 0x100 | 0x200 | 0x300 |
                    0x400 | 0x500 | 0x600 | 0x700 |
                    0x800 | 0x900 | 0xA00 | 0xB00 |
                    0xC00 | 0xD00 =>
                        self.wram[(addr & 0x1FFF) as usize],

                    // Graphics: object attribute memory
                    // OAM is 160 bytes, remaining bytes read as 0
                    0xE00 =>
                        if addr < 0xFEA0 {
                            0x00 // TODO: Hook up to GPU
                        } else {
                            0x00
                        },

                    // Zero page
                    0xF00 =>
                        if addr >= 0xFF80 {
                            self.zram[(addr & 0x7F) as usize]
                        } else {
                            // TODO: I/O control handling
                            0x00
                        },

                    _ => 0x00
                }
            }

            _ => 0x00
        }
    }

    pub fn wb(&mut self, addr: u16, val: u8) {
        match addr & 0xF000 {
            // ROM bank 0
            0x0000 | 0x1000 | 0x2000 | 0x3000 => {}

            // ROM bank 1
            0x4000 | 0x5000 | 0x6000 | 0x7000 => {}

            // VRAM
            0x8000 | 0x9000 => {
                // TODO: Set GPU memory
            }

            // External RAM
            0xA000 | 0xB000 => {
                self.eram[(addr & 0x1FFF) as usize] = val;
            }

            // Working RAM and echo
            0xC000 | 0xD000 | 0xE000 => {
                self.wram[(addr & 0x1FFF) as usize] = val;
            }

            // Everything else
            0xF000 => {
                match addr & 0x0F00 {
                    // Echo RAM
                    0x000 | 0x100 | 0x200 | 0x300 |
                    0x400 | 0x500 | 0x600 | 0x700 |
                    0x800 | 0x900 | 0xA00 | 0xB00 |
                    0xC00 | 0xD00 => {
                        self.wram[(addr & 0x1FFF) as usize] = val;
                    }

                    // OAM
                    0xE00 => {
                        if (addr & 0xFF) < 0xA0 {
                            // TODO: Set GPU memory
                        }
                        // TODO: Set GPU memory
                    }

                    // Zeropage RAM, I/O
                    0xF00 => {
                        if addr > 0xFF7F {
                            self.zram[(addr & 0x7F) as usize] = val;
                        } else {
                            match addr & 0xF0 {
                                // TODO: I/O
                                _ => {}
                            }
                        }
                    }

                    _ => {}
                }
            }

            _ => {}
        }
    }

    pub fn rw(&self, addr: u16) -> u16 {
        ((self.rb(addr) as u16) << 8) | (self.rb(addr + 1) as u16)
    }

    pub fn ww(&mut self, addr: u16, val: u16) {
        self.wb(addr, (val >> 8) as u8);
        self.wb(addr + 1, val as u8);
    }

    pub fn load_cartridge(&mut self, rom: Vec<u8>) {
        self.rom = rom;

        match self.rom[0x0147] {
            _ => {}
        }
    }
}

#[cfg(test)]
mod test {
    use mmu::Mmu;

    fn init() -> Mmu { Mmu::new() }

    #[test]
    fn read_byte() {
        let mut m = init();
        let (addr, val) = (0xcae0, 0x31);
        m.wb(addr, val);
        assert_eq!(m.rb(addr), val);
    }

    #[test]
    fn read_word() {
        let mut m = init();
        let (addr, val) = (0xa042, 0xdead);
        m.ww(addr, val);
        assert_eq!(m.rw(addr), val);
    }
}
