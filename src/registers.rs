// The following are the four flags set by certain arithmetic operations
pub const Z: u8 = 0x80; // Zero: Set if the result is zero.
pub const N: u8 = 0x40; // Operation: Set if the last operation was a subtraction.
pub const H: u8 = 0x20; // Half-carry: Set if there was carry from the low nibble to the high. In the high byte for 16 bit operations.
pub const C: u8 = 0x10; // Carry: Set if last options overflowed or underflowed.

pub struct Registers {
    pub a: u8, pub b: u8, pub c: u8, pub d: u8, pub e: u8, pub h: u8, pub l: u8, pub f: u8,
    pub pc: u16, pub sp: u16,
    pub m: u32, pub t: u32,
}

macro_rules! pair {
    ( $r: ident, $reg1: ident, $reg2:ident ) => {
        (($r.$reg1 as u16) << 8 ) | ($r.$reg2 as u16)
    }
}

impl Registers {
    pub fn new() -> Registers {
        Registers {
            a: 0, b: 0, c: 0, d: 0, e: 0, h: 0, l: 0, f: 0,
            pc: 0, sp: 0,
            m: 0, t: 0,
        }
    }

    pub fn reset(&mut self) {
        self.a = 0; self.b = 0; self.c = 0; self.d = 0;
        self.e = 0; self.h = 0; self.l = 0; self.f = 0;
        self.pc = 0; self.sp = 0;
        self.m = 0; self.t = 0;
    }

    pub fn bc(&self) -> u16 { pair!(self, b, c) }
    pub fn de(&self) -> u16 { pair!(self, d, e) }
    pub fn hl(&self) -> u16 { pair!(self, h, l) }

    // popw
    // Pop a word off the stack.
    pub fn popw(&mut self) -> u16 {
        let sp = self.sp;
        self.sp += 2;
        sp
    }

    // popb
    // Pop a byte off the stack.
    pub fn popb(&mut self) -> u16 {
        let sp = self.sp;
        self.sp += 1;
        sp
    }

    // pushw
    // Push a word onto the stack.
    pub fn pushw(&mut self) -> u16 {
        self.sp -= 2;
        self.sp
    }

    // pushb
    // Push a byte onto the stack.
    pub fn pushb(&mut self) -> u16 {
        self.sp -= 1;
        self.sp
    }

    // z
    // Checks if Z flag is set.
    pub fn z(&self) -> bool {
        self.f & Z == Z
    }

    // nz
    // Checks if Z flag is not set.
    pub fn nz(&self) -> bool {
        !self.z()
    }
}
