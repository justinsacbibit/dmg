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
}
