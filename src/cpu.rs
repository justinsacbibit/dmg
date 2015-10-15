use super::mmu::Mmu;
use super::registers::Registers;

// The following are the four flags set by certain arithmetic operations
const Z: u8 = 0x80; // Zero: Set if the result is zero.
const N: u8 = 0x40; // Operation: Set if the last operation was a subtraction.
const H: u8 = 0x20; // Half-carry: Set if there was carry from the low nibble to the high. In the high byte for 16 bit operations.
const C: u8 = 0x10; // Carry: Set if last options overflowed or underflowed.

pub struct Cpu {
    // Clock
    m: u32,
    t: u32,

    r: Registers,
}

macro_rules! set_clock( ( $cpu: ident, $m: expr ) => ({
    $cpu.m = $m;
    $cpu.t = $m * 4;
}) );

macro_rules! set_r_clock( ( $cpu: ident, $m: expr ) => ({
    $cpu.r.m = $m;
    $cpu.r.t = $m * 4;
}) );

impl Cpu {
    pub fn new() -> Cpu {
        Cpu {
            m: 0,
            t: 0,
            r: Registers::new(),
        }
    }

    fn bump(&mut self) -> u16 {
        let ret = self.r.pc;
        self.r.pc += 1;
        ret
    }

    pub fn reset(&mut self) {
        self.m = 0; self.t = 0;
        self.r.reset();
    }

    pub fn exec(&mut self, m: &mut Mmu) -> u32 {
        let op = m.rb(self.bump());
        let m_cycles = self.exec_instr(op, m);
        self.m += self.r.m;
        self.t += self.r.t;

        m_cycles
    }

    fn exec_instr(&mut self, instr: u8, m: &mut Mmu) -> u32 {
        macro_rules! ld_nn {
            ($reg1: ident, $reg2: ident) => ({
                self.r.$reg1 = m.rb(self.bump());
                self.r.$reg2 = m.rb(self.bump());
                3
            })
        }
        macro_rules! inc_ss {
            ($reg1: ident, $reg2: ident) => ({
                self.r.$reg2 += 1;
                if self.r.$reg2 == 0 {
                    self.r.$reg1 += 1;
                }
                2
            })
        }
        macro_rules! inc {
            ($reg: ident) => ({
                self.r.$reg += 1;
                self.r.f &= C;
                self.r.f |= if self.r.$reg == 0 { Z } else { 0 };
                self.r.f |= if self.r.$reg & 0xf == 0 { H } else { 0 };
                1
            })
        }
        macro_rules! dec {
            ($reg: ident) => ({
                self.r.$reg -= 1;
                self.r.f &= C;
                self.r.f |= N;
                self.r.f |= if self.r.$reg == 0 { Z } else { 0 };
                self.r.f |= if self.r.$reg & 0xf == 0xf { H } else { 0 };
                1
            })
        }
        macro_rules! ld_n {
            ($reg: ident) => ({
                self.r.$reg = m.rb(self.bump());
                2
            })
        }
        macro_rules! rlc_r {
            ($reg: ident) => ({
                let carry = if self.r.$reg & 0x80 == 0x80 { 0x1 } else { 0x0 };
                self.r.$reg = (self.r.$reg << 1) | carry;
                self.r.f = carry << 4;
                2
            })
        }
        macro_rules! add_hl {
            ($reg1: ident, $reg2: ident) => ({
                self.r.f &= Z;
                // TODO: Cleanup
                self.r.f |= if ((((self.r.h as u16) << 8) | self.r.l as u16) & 0xfff) + ((((self.r.$reg1 as u16) << 8) | self.r.$reg2 as u16) & 0xfff) > 0xfff {
                    H
                } else {
                    0
                };
                let result = ((self.r.h as u32) << 8 | self.r.l as u32)
                    + ((self.r.$reg1 as u32) << 8 | self.r.$reg2 as u32);
                self.r.f |= if result > 0xffff { C } else { 0 };
                self.r.h = (result >> 8) as u8;
                self.r.l = result as u8;
                2
            })
        }
        macro_rules! ld_a {
            ($addr: expr) => ({
                self.r.a = m.rb($addr);
                2
            })
        }
        macro_rules! dec_ss {
            ($reg1: ident, $reg2: ident) => ({
                self.r.$reg2 -= 1;
                if self.r.$reg2 == 0xff {
                    self.r.$reg1 -= 1;
                }
                2
            })
        }
        macro_rules! add_r {
            ($reg: ident) => ({
                self.r.f = 0;
                if (self.r.a & 0xF) + (self.r.$reg & 0xF) > 0xF {
                    // Half carry
                    self.r.f |= H;
                }
                if (self.r.a as u32) + (self.r.$reg as u32) > 255 {
                    // Carry
                    self.r.f |= C;
                }
                self.r.a += self.r.$reg;
                if self.r.a == 0 {
                    // Zero
                    self.r.f |= Z;
                }
                1
            })
        }

        let m_cycle = match instr {
            0x00 => 1, // NOP
            0x01 => ld_nn!(b, c), // LD BC, nn
            0x02 => { m.wb(self.r.bc(), self.r.a); 2 } // LD (BC), A
            0x03 => inc_ss!(b, c), // INC BC
            0x04 => inc!(b), // INC B
            0x05 => dec!(b), // DEC B
            0x06 => ld_n!(b), // LD B, n
            0x07 => { // RLCA
                let carry = if self.r.a & 0x80 == 0x80 { 0x1 } else { 0x0 };
                self.r.a = (self.r.a << 1) | carry;
                self.r.f = carry << 4;
                1
            }
            0x08 => { let addr = m.rw(self.r.pc); m.ww(addr, self.r.sp); self.r.pc += 2; 5 }
            0x09 => add_hl!(b, c), // ADD HL, BC
            0x0a => ld_a!(self.r.bc()), // LD A, (BC)
            0x0b => dec_ss!(b, c), // DEC BC
            0x0c => inc!(c), // INC C
            0x0d => dec!(c), // DEC C
            0x0e => ld_n!(c), // LD C, n
            0x0f => { // RRCA
                let carry = if self.r.a & 0x1 == 0x1 { 0x1 } else { 0x0 };
                self.r.a = (self.r.a >> 1) | (carry << 7);
                self.r.f = carry << 4;
                1
            }
            0x10 => { self.r.pc += 1; 1 }
            0x11 => ld_nn!(d, e), // LD DE, nn
            0x12 => { m.wb(self.r.de(), self.r.a); 2 } // LD (DE), A
            0x80 => add_r!(b), // ADD A, B

            _ => 0
        };

        set_r_clock!(self, m_cycle);

        m_cycle
    }
}

#[cfg(test)]
mod test {
    use cpu::Cpu;
    use cpu::{Z, N, H, C};
    use mmu::Mmu;

    fn init() -> (Cpu, Mmu) {
        let (mut c, m) = (Cpu::new(), Mmu::new());
        c.r.pc = 0xE000; // wram
        (c, m)
    }

    #[test]
    fn reset() {
        let (mut c, _) = init();
        c.r.a = 1;
        c.r.b = 1;
        c.r.c = 1;
        c.r.d = 1;
        c.r.e = 1;
        c.r.h = 1;
        c.r.l = 1;
        c.r.f = 1;
        c.r.pc = 1;
        c.r.sp = 1;
        c.m = 2;
        c.t = 8;

        // Sanity check
        assert_eq!(c.m, 2);
        assert_eq!(c.t, 8);

        c.reset();

        assert_eq!(c.r.a, 0);
        assert_eq!(c.r.b, 0);
        assert_eq!(c.r.c, 0);
        assert_eq!(c.r.d, 0);
        assert_eq!(c.r.e, 0);
        assert_eq!(c.r.h, 0);
        assert_eq!(c.r.l, 0);
        assert_eq!(c.r.f, 0);
        assert_eq!(c.r.pc, 0);
        assert_eq!(c.r.sp, 0);
        assert_eq!(c.m, 0);
        assert_eq!(c.t, 0);
    }

    fn op(c: &mut Cpu, m: &mut Mmu, instr: u8, pc_diff: u16, m_cycles: u32) {
        let pc = c.r.pc;
        c.m = 4;
        c.t = 16;
        m.wb(pc, instr);
        let m_cycle = c.exec(m);
        assert_eq!(m_cycle, m_cycles);
        assert_eq!(c.r.pc, pc + pc_diff);
        assert_eq!(c.r.m, m_cycles);
        assert_eq!(c.r.t, m_cycles * 4);
        assert_eq!(c.m, 4 + m_cycles);
        assert_eq!(c.t, 16 + m_cycles * 4);
    }

    #[test]
    fn nop() {
        let (mut c, mut m) = init();
        op(&mut c, &mut m, 0x00, 1, 1);
    }

    #[test]
    fn ld_bc_nn() {
        let (mut c, mut m) = init();
        m.ww(c.r.pc + 1, 0xe23f);
        op(&mut c, &mut m, 0x01, 3, 3);
        assert_eq!(c.r.b, 0xe2);
        assert_eq!(c.r.c, 0x3f);
    }

    #[test]
    fn ld_bc_a() {
        let (mut c, mut m) = init();
        c.r.a = 0x34;
        c.r.b = 0xD0;
        c.r.c = 0xED;
        op(&mut c, &mut m, 0x02, 1, 2);
        assert_eq!(m.rb(0xD0ED), 0x34);
    }

    #[test]
    fn inc_bc() {
        let (mut c, mut m) = init();
        c.r.b = 0x20;
        c.r.c = 0x43;
        op(&mut c, &mut m, 0x03, 1, 2);
        assert_eq!(c.r.b, 0x20);
        assert_eq!(c.r.c, 0x44);
    }

    #[test]
    fn inc_bc_carry() {
        let (mut c, mut m) = init();
        c.r.b = 0x20;
        c.r.c = 0xff;
        op(&mut c, &mut m, 0x03, 1, 2);
        assert_eq!(c.r.c, 0x00);
        assert_eq!(c.r.b, 0x21);
    }

    #[test]
    fn inc_b() {
        let (mut c, mut m) = init();
        c.r.b = 0x20;
        op(&mut c, &mut m, 0x04, 1, 1);
        assert_eq!(c.r.b, 0x21);
        assert_eq!(c.r.f, 0x00);
    }

    #[test]
    fn inc_b_zero() {
        let (mut c, mut m) = init();
        c.r.b = 0xff;
        op(&mut c, &mut m, 0x04, 1, 1);
        assert_eq!(c.r.b, 0x00);
        assert_eq!(c.r.f, Z | H);
    }

    #[test]
    fn inc_b_half_carry() {
        let (mut c, mut m) = init();
        c.r.b = 0x0f;
        op(&mut c, &mut m, 0x04, 1, 1);
        assert_eq!(c.r.b, 0x10);
        assert_eq!(c.r.f, H);
    }

    #[test]
    fn dec_b() {
        let (mut c, mut m) = init();
        c.r.b = 0x20;
        op(&mut c, &mut m, 0x05, 1, 1);
        assert_eq!(c.r.b, 0x1F);
        assert_eq!(c.r.f, N | H);
    }

    #[test]
    fn dec_b_zero() {
        let (mut c, mut m) = init();
        c.r.b = 0x01;
        op(&mut c, &mut m, 0x05, 1, 1);
        assert_eq!(c.r.b, 0x00);
        assert_eq!(c.r.f, Z | N);
    }

    #[test]
    fn dec_b_half_carry() {
        let (mut c, mut m) = init();
        c.r.b = 0x00;
        op(&mut c, &mut m, 0x05, 1, 1);
        assert_eq!(c.r.b, 0xff);
        assert_eq!(c.r.f, N | H);
    }

    #[test]
    fn ld_b_n() {
        let (mut c, mut m) = init();
        m.wb(c.r.pc + 1, 0x20);
        op(&mut c, &mut m, 0x06, 2, 2);
        assert_eq!(c.r.b, 0x20);
    }

    #[test]
    fn rlc_a() {
        let (mut c, mut m) = init();
        c.r.a = 0x7e;
        op(&mut c, &mut m, 0x07, 1, 1);
        assert_eq!(c.r.a, 0xfc);
        assert_eq!(c.r.f, 0);
    }

    #[test]
    fn rlc_a_carry() {
        let (mut c, mut m) = init();
        c.r.a = 0x8e;
        op(&mut c, &mut m, 0x07, 1, 1);
        assert_eq!(c.r.a, 0x1d);
        assert_eq!(c.r.f, C);
    }

    #[test]
    fn ld_nn_sp() {
        let (mut c, mut m) = init();
        m.ww(c.r.pc + 1, 0xa000);
        c.r.sp = 0xabcd;
        op(&mut c, &mut m, 0x08, 3, 5);
        assert_eq!(m.rw(0xa000), 0xabcd);
    }

    #[test]
    fn add_hl_bc() {
        let (mut c, mut m) = init();
        c.r.h = 0x12;
        c.r.l = 0x34;
        c.r.b = 0x56;
        c.r.c = 0x78;
        op(&mut c, &mut m, 0x09, 1, 2);
        assert_eq!(c.r.h, 0x68);
        assert_eq!(c.r.l, 0xac);
        assert_eq!(c.r.f, 0x0);
    }

    #[test]
    fn add_hl_bc_no_half_carry() {
        let (mut c, mut m) = init();
        c.r.h = 0x12;
        c.r.l = 0xff;
        c.r.b = 0x23;
        c.r.c = 0x01;
        op(&mut c, &mut m, 0x09, 1, 2);
        assert_eq!(c.r.h, 0x36);
        assert_eq!(c.r.l, 0x00);
        assert_eq!(c.r.f, 0x0);
    }

    #[test]
    fn add_hl_bc_half_carry() {
        let (mut c, mut m) = init();
        c.r.h = 0x1f;
        c.r.l = 0xe0;
        c.r.b = 0x21;
        c.r.c = 0x01;
        op(&mut c, &mut m, 0x09, 1, 2);
        assert_eq!(c.r.h, 0x40);
        assert_eq!(c.r.l, 0xe1);
        assert_eq!(c.r.f, H);
    }

    #[test]
    fn add_hl_bc_carry() {
        let (mut c, mut m) = init();
        c.r.h = 0xff;
        c.r.l = 0xff;
        c.r.b = 0x00;
        c.r.c = 0x02;
        op(&mut c, &mut m, 0x09, 1, 2);
        assert_eq!(c.r.h, 0x00);
        assert_eq!(c.r.l, 0x01);
        assert_eq!(c.r.f, H | C);
    }

    #[test]
    fn add_hl_bc_only_carry() {
        let (mut c, mut m) = init();
        c.r.h = 0xf0;
        c.r.l = 0x12;
        c.r.b = 0x11;
        c.r.c = 0x02;
        op(&mut c, &mut m, 0x09, 1, 2);
        assert_eq!(c.r.h, 0x01);
        assert_eq!(c.r.l, 0x14);
        assert_eq!(c.r.f, C);
    }

    #[test]
    fn ld_a_bc() {
        let (mut c, mut m) = init();
        c.r.b = 0xd0;
        c.r.c = 0x00;
        m.wb(0xd000, 0x4e);
        op(&mut c, &mut m, 0x0a, 1, 2);
        assert_eq!(c.r.a, 0x4e);
    }

    #[test]
    fn dec_bc() {
        let (mut c, mut m) = init();
        c.r.b = 0x12;
        c.r.c = 0x34;
        op(&mut c, &mut m, 0x0b, 1, 2);
        assert_eq!(c.r.b, 0x12);
        assert_eq!(c.r.c, 0x33);
    }

    #[test]
    fn dec_bc_half_carry() {
        let (mut c, mut m) = init();
        c.r.b = 0x12;
        c.r.c = 0x00;
        op(&mut c, &mut m, 0x0b, 1, 2);
        assert_eq!(c.r.b, 0x11);
        assert_eq!(c.r.c, 0xff);
    }

    #[test]
    fn dec_bc_underflow() {
        let (mut c, mut m) = init();
        c.r.b = 0x00;
        c.r.c = 0x00;
        op(&mut c, &mut m, 0x0b, 1, 2);
        assert_eq!(c.r.b, 0xff);
        assert_eq!(c.r.c, 0xff);
    }

    #[test]
    fn inc_c() {
        let (mut c, mut m) = init();
        c.r.c = 0x20;
        op(&mut c, &mut m, 0x0c, 1, 1);
        assert_eq!(c.r.c, 0x21);
        assert_eq!(c.r.f, 0x00);
    }

    #[test]
    fn inc_c_zero() {
        let (mut c, mut m) = init();
        c.r.c = 0xff;
        op(&mut c, &mut m, 0x0c, 1, 1);
        assert_eq!(c.r.c, 0x00);
        assert_eq!(c.r.f, Z | H);
    }

    #[test]
    fn inc_c_half_carry() {
        let (mut c, mut m) = init();
        c.r.c = 0x0f;
        op(&mut c, &mut m, 0x0c, 1, 1);
        assert_eq!(c.r.c, 0x10);
        assert_eq!(c.r.f, H);
    }

    #[test]
    fn dec_c() {
        let (mut c, mut m) = init();
        c.r.c = 0x20;
        op(&mut c, &mut m, 0x0d, 1, 1);
        assert_eq!(c.r.c, 0x1F);
        assert_eq!(c.r.f, N | H);
    }

    #[test]
    fn dec_c_zero() {
        let (mut c, mut m) = init();
        c.r.c = 0x01;
        op(&mut c, &mut m, 0x0d, 1, 1);
        assert_eq!(c.r.c, 0x00);
        assert_eq!(c.r.f, Z | N);
    }

    #[test]
    fn dec_c_half_carry() {
        let (mut c, mut m) = init();
        c.r.c = 0x00;
        op(&mut c, &mut m, 0x0d, 1, 1);
        assert_eq!(c.r.c, 0xff);
        assert_eq!(c.r.f, N | H);
    }

    #[test]
    fn ld_c_n() {
        let (mut c, mut m) = init();
        m.wb(c.r.pc + 1, 0x3e);
        op(&mut c, &mut m, 0x0e, 2, 2);
        assert_eq!(c.r.c, 0x3e);
    }

    #[test]
    fn rrc_a() {
        let (mut c, mut m) = init();
        c.r.a = 0x7e;
        op(&mut c, &mut m, 0x0f, 1, 1);
        assert_eq!(c.r.a, 0x3f);
        assert_eq!(c.r.f, 0);
    }

    #[test]
    fn rrc_a_carry() {
        let (mut c, mut m) = init();
        c.r.a = 0xd1;
        op(&mut c, &mut m, 0x0f, 1, 1);
        assert_eq!(c.r.a, 0xe8);
        assert_eq!(c.r.f, C);
    }

    #[test]
    fn stop() {
        let (mut c, mut m) = init();
        op(&mut c, &mut m, 0x10, 2, 1);
    }

    #[test]
    fn ld_de_nn() {
        let (mut c, mut m) = init();
        m.ww(c.r.pc + 1, 0xfe12);
        op(&mut c, &mut m, 0x11, 3, 3);
        assert_eq!(c.r.d, 0xfe);
        assert_eq!(c.r.e, 0x12);
    }

    #[test]
    fn ld_de_a() {
        let (mut c, mut m) = init();
        c.r.a = 0xab;
        c.r.d = 0xd0;
        c.r.e = 0x04;
        op(&mut c, &mut m, 0x12, 1, 2);
        assert_eq!(m.rb(0xd004), 0xab);
    }

    /*
    #[test]
    fn add_a_b() {
        let (mut c, mut m) = init();
        c.r.a = 0x02;
        c.r.b = 0x01;
        c.r.f = 0x80 | 0x40 | 0x20 | 0x10;
        op(&mut c, &mut m, 0x80, 1, 1);
        assert_eq!(c.r.a, 0x03);
        assert_eq!(c.r.f, 0x00);
        // TODO: Check all flags
    }
    */
}
