use super::mmu;

pub struct Cpu {
    // Time clock
    m: u32,

    // Registers
    a: u8, b: u8, c: u8, d: u8, e: u8, h: u8, l: u8, f: u8,
    pub pc: u16,
    sp: u16,
    // Clock for last instr
    r_m: u32,
}

impl Cpu {
    pub fn new() -> Cpu {
        Cpu {
            m: 0u32,
            a: 0, b: 0, c: 0, d: 0, e: 0, h: 0, l: 0, f: 0,
            pc: 0,
            sp: 0,
            r_m: 0,
        }
    }

    fn bump(&mut self) -> u16 {
        let ret = self.pc;
        self.pc += 1;
        ret
    }

    pub fn reset(&mut self) {
        self.a = 0; self.b = 0; self.c = 0; self.d = 0;
        self.e = 0; self.h = 0; self.l = 0; self.f = 0;
        self.pc = 0;
        self.sp = 0;
        self.r_m = 0;
        self.m = 0;
    }

    pub fn exec(&mut self, instr: u8, m: &mut mmu::Mmu) -> u32 {
        macro_rules! ld_nn( ($reg1: ident, $reg2: ident) => ({
            self.$reg2 = m.rb(self.bump());
            self.$reg1 = m.rb(self.bump());
            3
        }) );

        match instr {
            0x00 => 1,
            0x01 => ld_nn!(b, c),

            _ => 0
        }
    }
}

#[cfg(test)]
mod test {
    use cpu;
    use mmu;

    fn init() -> (cpu::Cpu, mmu::Mmu) {
        (cpu::Cpu::new(), mmu::Mmu::new())
    }

    #[test]
    fn nop() {
        let (mut c, mut m) = init();
        let result = c.exec(0x00, &mut m);
        assert_eq!(result, 1);
        assert_eq!(c.pc, 0);
    }

    #[test]
    fn ln_bc_nn() {
        let (mut c, mut m) = init();
        let result = c.exec(0x01, &mut m);
        assert_eq!(result, 3);
        assert_eq!(c.pc, 2);
        // TODO: Check the contents of the registers
    }
}
