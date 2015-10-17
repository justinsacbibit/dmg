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
        macro_rules! add_hl_helper {
            ($val: expr) => ({
                self.r.f &= Z;
                // TODO: Cleanup
                self.r.f |= if ((((self.r.h as u16) << 8) | self.r.l as u16) & 0xfff) + ($val & 0xfff) > 0xfff {
                    H
                } else {
                    0
                };
                let result = ((self.r.h as u32) << 8 | self.r.l as u32)
                    + $val as u32;
                self.r.f |= if result > 0xffff { C } else { 0 };
                self.r.h = (result >> 8) as u8;
                self.r.l = result as u8;
                2
            })
        }
        macro_rules! add_hl {
            ($reg1: ident, $reg2: ident) => ({
                add_hl_helper!(((self.r.$reg1 as u16) << 8) | self.r.$reg2 as u16)
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
        macro_rules! jr_e {
            ($cond: expr) => ({
                let e = m.rb(self.bump()) as i8;
                if $cond {
                    if e < 0 { self.r.pc -= (-e) as u16; } else { self.r.pc += e as u16; }
                    3
                } else {
                    2
                }
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
            0x13 => inc_ss!(d, e), // INC DE
            0x14 => inc!(d), // INC D
            0x15 => dec!(d), // DEC D
            0x16 => ld_n!(d), // LD D, n
            0x17 => { // RLA
                self.r.f &= C;
                let carry = if self.r.a & 0x80 == 0x80 { C } else { 0x0 };
                self.r.a = (self.r.a << 1) | if self.r.f == C { 0x1 } else { 0x0 };
                self.r.f = carry;
                1
            }
            0x18 => jr_e!(true), // JR e
            0x19 => add_hl!(d, e), // ADD HL, DE
            0x1a => ld_a!(self.r.de()), // LD A, (DE)
            0x1b => dec_ss!(d, e), // DEC DE
            0x1c => inc!(e), // INC E
            0x1d => dec!(e), // DEC E
            0x1e => ld_n!(e), // LD E, n
            0x1f => { // RRA
                self.r.f &= C;
                let carry = if self.r.a & 0x1 == 0x1 { C } else { 0x0 };
                self.r.a = (self.r.a >> 1) | if self.r.f == C { 0x80 } else { 0x0 };
                self.r.f = carry;
                1
            }
            0x20 => jr_e!(self.r.f & Z != Z),
            0x21 => ld_nn!(h, l), // LD HL, nn
            0x22 => { // LDI (HL), A
                m.wb(self.r.hl(), self.r.a);
                inc_ss!(h, l)
            }
            0x23 => inc_ss!(h, l), // INC HL
            0x24 => inc!(h), // INC H
            0x25 => dec!(h), // DEC H
            0x26 => ld_n!(h), // LD H
            0x27 => { // DAA
                // This beast is missing tests
                let mut a = self.r.a as u16;
                if self.r.f & N != N {
                    if self.r.f & H == H || (a & 0xf) > 0x9 {
                        a += 0x06;
                    }
                    if self.r.f & C == C || a > 0x9f {
                        a += 0x60;
                    }
                } else {
                    if self.r.f & H == H {
                        a = (a - 6) & 0xff;
                    }
                    if self.r.f & C == C {
                        a -= 0x60;
                    }
                }
                self.r.f &= !(Z | H | C);
                if (a & 0x100) == 0x100 {
                    self.r.f |= C;
                }
                if a == 0 {
                    self.r.f |= Z;
                }
                self.r.a = a as u8;
                1
            }
            0x28 => jr_e!(self.r.f & Z == Z), // JR Z, e
            0x29 => add_hl!(h, l), // ADD HL, HL
            0x2a => { // LDI A, (HL)
                self.r.a = m.rb(self.r.hl());
                inc_ss!(h, l)
            }
            0x2b => dec_ss!(h, l), // DEC HL
            0x2c => inc!(l), // INC L
            0x2d => dec!(l), // DEC L
            0x2e => ld_n!(l), // LD L, n
            0x2f => { // CPL
                self.r.a = !self.r.a;
                self.r.f = N | H;
                1
            }
            0x30 => jr_e!(self.r.f & C != C), // JR NC, e
            0x31 => { // LD SP, nn
                self.r.sp = m.rw(self.bump());
                self.bump();
                3
            }
            0x32 => { // LDD (HL), A
                m.wb(self.r.hl(), self.r.a);
                dec_ss!(h, l)
            }
            0x33 => { // INC SP
                self.r.sp += 1;
                2
            }
            0x34 => { // INC (HL)
                let mut value = m.rb(self.r.hl());
                value += 1;
                self.r.f &= C;
                self.r.f |= if value == 0 { Z } else { 0 };
                self.r.f |= if value & 0xf == 0 { H } else { 0 };
                m.wb(self.r.hl(), value);
                3
            }
            0x35 => { // DEC (HL)
                let mut value = m.rb(self.r.hl());
                value -= 1;
                self.r.f &= C;
                self.r.f |= N;
                self.r.f |= if value == 0 { Z } else { 0 };
                self.r.f |= if value & 0xf == 0xf { H } else { 0 };
                m.wb(self.r.hl(), value);
                3
            }
            0x36 => { // LD (HL), n
                let value = m.rb(self.bump());
                m.wb(self.r.hl(), value);
                3
            }
            0x37 => { self.r.f &= Z; self.r.f |= C; 1 },
            0x38 => jr_e!(self.r.f & C == C), // JR C, e
            0x39 => add_hl_helper!(self.r.sp), // ADD HL, SP
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

    fn op(c: &mut Cpu, m: &mut Mmu, instr: u8, pc_diff: i16, m_cycles: u32) {
        let pc = c.r.pc;
        c.m = 4;
        c.t = 16;
        m.wb(pc, instr);
        let m_cycle = c.exec(m);
        assert_eq!(m_cycle, m_cycles);
        if pc_diff >= 0 {
            assert_eq!(c.r.pc, pc + (pc_diff as u16));
        } else {
            assert_eq!(c.r.pc, pc - ((-pc_diff) as u16));
        }
        assert_eq!(c.r.m, m_cycles);
        assert_eq!(c.r.t, m_cycles * 4);
        assert_eq!(c.m, 4 + m_cycles);
        assert_eq!(c.t, 16 + m_cycles * 4);
    }

    macro_rules! op {
        ($reg: ident, $initial: expr, $op_code: expr, $pc: expr, $m: expr, $expect: expr, $f: expr) => ({
            let (mut c, mut m) = init();
            c.r.$reg = $initial;
            op(&mut c, &mut m, $op_code, $pc, $m);
            assert_eq!(c.r.$reg, $expect);
            assert_eq!(c.r.f, $f);
        })
    }

    macro_rules! inc_r_helper {
        ($reg: ident, $initial: expr, $op: expr, $expect: expr, $f: expr) => ({
            op!($reg, $initial, $op, 1, 1, $expect, $f);
        })
    }

    macro_rules! inc_r {
        ($reg: ident, $op: expr) => ({
            inc_r_helper!($reg, 0x00, $op, 0x01, 0x00);
        })
    }

    macro_rules! inc_r_zero {
        ($reg: ident, $op: expr) => ({
            inc_r_helper!($reg, 0xff, $op, 0x00, Z | H);
        })
    }

    macro_rules! inc_r_half_carry {
        ($reg: ident, $op: expr) => ({
            inc_r_helper!($reg, 0x0f, $op, 0x10, H);
        })
    }

    macro_rules! dec_r_helper {
        ($reg: ident, $initial: expr, $op: expr, $expect: expr, $f: expr) => ({
            op!($reg, $initial, $op, 1, 1, $expect, $f);
        })
    }

    macro_rules! dec_r {
        ($reg: ident, $op: expr) => ({
            dec_r_helper!($reg, 0x20, $op, 0x1f, N | H);
        })
    }

    macro_rules! dec_r_zero {
        ($reg: ident, $op: expr) => ({
            dec_r_helper!($reg, 0x01, $op, 0x00, Z | N);
        })
    }

    macro_rules! dec_r_half_carry {
        ($reg: ident, $op: expr) => ({
            dec_r_helper!($reg, 0x00, $op, 0xff, N | H);
        })
    }

    macro_rules! ld_rr_nn {
        ($reg1: ident, $reg2: ident, $op: expr) => ({
            let (mut c, mut m) = init();
            m.ww(c.r.pc + 1, 0xe23f);
            op(&mut c, &mut m, $op, 3, 3);
            assert_eq!(c.r.$reg1, 0xe2);
            assert_eq!(c.r.$reg2, 0x3f);
        })
    }

    macro_rules! ld_rr_r {
        ($reg1: ident, $reg2: ident, $src: ident, $op: expr) => ({
            let (mut c, mut m) = init();
            c.r.$src = 0x34;
            c.r.$reg1 = 0xD0;
            c.r.$reg2 = 0xED;
            op(&mut c, &mut m, $op, 1, 2);
            assert_eq!(m.rb(0xD0ED), 0x34);
        })
    }

    macro_rules! inc_rr {
        ($reg1: ident, $reg2: ident, $op: expr) => ({
            let (mut c, mut m) = init();
            c.r.$reg1 = 0x20;
            c.r.$reg2 = 0x43;
            op(&mut c, &mut m, $op, 1, 2);
            assert_eq!(c.r.$reg1, 0x20);
            assert_eq!(c.r.$reg2, 0x44);
            assert_eq!(c.r.f, 0x00);
        })
    }

    macro_rules! inc_rr_half_carry {
        ($reg1: ident, $reg2: ident, $op: expr) => ({
            let (mut c, mut m) = init();
            c.r.$reg1 = 0x00;
            c.r.$reg2 = 0xff;
            op(&mut c, &mut m, $op, 1, 2);
            assert_eq!(c.r.$reg1, 0x01);
            assert_eq!(c.r.$reg2, 0x00);
            assert_eq!(c.r.f, 0x00);
        })
    }

    macro_rules! inc_rr_overflow {
        ($reg1: ident, $reg2: ident, $op: expr) => ({
            let (mut c, mut m) = init();
            c.r.$reg1 = 0xff;
            c.r.$reg2 = 0xff;
            op(&mut c, &mut m, $op, 1, 2);
            assert_eq!(c.r.$reg1, 0x00);
            assert_eq!(c.r.$reg2, 0x00);
            assert_eq!(c.r.f, 0x00);
        })
    }

    macro_rules! ld_r_n {
        ($reg: ident, $op: expr) => ({
            let (mut c, mut m) = init();
            m.wb(c.r.pc + 1, 0x20);
            op(&mut c, &mut m, $op, 2, 2);
            assert_eq!(c.r.$reg, 0x20);
        })
    }

    macro_rules! add_hl_rr {
        ($reg1: ident, $reg2: ident, $op: expr) => ({
            let (mut c, mut m) = init();
            c.r.h = 0x12;
            c.r.l = 0x34;
            c.r.$reg1 = 0x56;
            c.r.$reg2 = 0x78;
            op(&mut c, &mut m, $op, 1, 2);
            assert_eq!(c.r.h, 0x68);
            assert_eq!(c.r.l, 0xac);
            assert_eq!(c.r.f, 0x0);
        })
    }

    macro_rules! add_hl_rr_half_carry {
        ($reg1: ident, $reg2: ident, $op: expr) => ({
            let (mut c, mut m) = init();
            c.r.h = 0x1f;
            c.r.l = 0xe0;
            c.r.$reg1 = 0x21;
            c.r.$reg2 = 0x01;
            op(&mut c, &mut m, $op, 1, 2);
            assert_eq!(c.r.h, 0x40);
            assert_eq!(c.r.l, 0xe1);
            assert_eq!(c.r.f, H);
        })
    }

    macro_rules! add_hl_rr_carry {
        ($reg1: ident, $reg2: ident, $op: expr) => ({
            let (mut c, mut m) = init();
            c.r.h = 0xff;
            c.r.l = 0xff;
            c.r.$reg1 = 0x00;
            c.r.$reg2 = 0x02;
            op(&mut c, &mut m, $op, 1, 2);
            assert_eq!(c.r.h, 0x00);
            assert_eq!(c.r.l, 0x01);
            assert_eq!(c.r.f, H | C);
        })
    }

    macro_rules! add_hl_rr_only_carry {
        ($reg1: ident, $reg2: ident, $op: expr) => ({
            let (mut c, mut m) = init();
            c.r.h = 0xf0;
            c.r.l = 0x12;
            c.r.$reg1 = 0x11;
            c.r.$reg2 = 0x02;
            op(&mut c, &mut m, $op, 1, 2);
            assert_eq!(c.r.h, 0x01);
            assert_eq!(c.r.l, 0x14);
            assert_eq!(c.r.f, C);
        })
    }

    macro_rules! ld_a_rr {
        ($reg1: ident, $reg2: ident, $op: expr) => ({
            let (mut c, mut m) = init();
            c.r.$reg1 = 0xd0;
            c.r.$reg2 = 0x00;
            m.wb(0xd000, 0x4e);
            op(&mut c, &mut m, $op, 1, 2);
            assert_eq!(c.r.a, 0x4e);
        })
    }

    macro_rules! dec_rr {
        ($reg1: ident, $reg2: ident, $op: expr) => ({
            let (mut c, mut m) = init();
            c.r.$reg1 = 0x12;
            c.r.$reg2 = 0x34;
            op(&mut c, &mut m, $op, 1, 2);
            assert_eq!(c.r.$reg1, 0x12);
            assert_eq!(c.r.$reg2, 0x33);
            assert_eq!(c.r.f, 0x00);
        })
    }

    macro_rules! dec_rr_half_carry {
        ($reg1: ident, $reg2: ident, $op: expr) => ({
            let (mut c, mut m) = init();
            c.r.$reg1 = 0x12;
            c.r.$reg2 = 0x00;
            op(&mut c, &mut m, $op, 1, 2);
            assert_eq!(c.r.$reg1, 0x11);
            assert_eq!(c.r.$reg2, 0xff);
            assert_eq!(c.r.f, 0x00);
        })
    }

    macro_rules! dec_rr_underflow {
        ($reg1: ident, $reg2: ident, $op: expr) => ({
            let (mut c, mut m) = init();
            c.r.$reg1 = 0x00;
            c.r.$reg2 = 0x00;
            op(&mut c, &mut m, $op, 1, 2);
            assert_eq!(c.r.$reg1, 0xff);
            assert_eq!(c.r.$reg2, 0xff);
            assert_eq!(c.r.f, 0x00);
        })
    }

    macro_rules! jr_e_helper {
        ($e: expr, $f: expr, $op: expr, $pc: expr, $t: expr) => ({
            let (mut c, mut m) = init();
            m.wb(c.r.pc + 1, $e);
            c.r.f = $f;
            op(&mut c, &mut m, $op, $pc, $t);
        })
    }

    macro_rules! jr_e {
        ($f: expr, $op: expr) => ({
            jr_e_helper!(0x02, $f, $op, 4, 3);
        })
    }

    macro_rules! jr_e_back {
        ($f: expr, $op: expr) => ({
            jr_e_helper!(0b11111100, $f, $op, -2, 3);
        })
    }

    macro_rules! jr_e_no_jump {
        ($f: expr, $op: expr) => ({
            jr_e_helper!(0x02, $f, $op, 2, 2);
        })
    }

    #[test]
    fn nop() {
        let (mut c, mut m) = init();
        op(&mut c, &mut m, 0x00, 1, 1);
    }

    #[test]
    fn ld_bc_nn() {
        ld_rr_nn!(b, c, 0x01);
    }

    #[test]
    fn ld_bc_a() {
        ld_rr_r!(b, c, a, 0x02);
    }

    #[test]
    fn inc_bc() {
        inc_rr!(b, c, 0x03);
    }

    #[test]
    fn inc_bc_half_carry() {
        inc_rr_half_carry!(b, c, 0x03);
    }

    #[test]
    fn inc_bc_overflow() {
        inc_rr_overflow!(b, c, 0x03);
    }

    #[test]
    fn inc_b() {
        inc_r!(b, 0x04);
    }

    #[test]
    fn inc_b_zero() {
        inc_r_zero!(b, 0x04);
    }

    #[test]
    fn inc_b_half_carry() {
        inc_r_half_carry!(b, 0x04);
    }

    #[test]
    fn dec_b() {
        dec_r!(b, 0x05);
    }

    #[test]
    fn dec_b_zero() {
        dec_r_zero!(b, 0x05);
    }

    #[test]
    fn dec_b_half_carry() {
        dec_r_half_carry!(b, 0x05);
    }

    #[test]
    fn ld_b_n() {
        ld_r_n!(b, 0x06);
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
    fn rlc_a_flags() {
        let (mut c, mut m) = init();
        c.r.a = 0x7e;
        c.r.f = Z | N | H | C;
        op(&mut c, &mut m, 0x07, 1, 1);
        assert_eq!(c.r.a, 0xfc);
        assert_eq!(c.r.f, 0x00);
    }

    #[test]
    fn ld_nn_sp() {
        let (mut c, mut m) = init();
        m.ww(c.r.pc + 1, 0xa2fe);
        c.r.sp = 0xabcd;
        op(&mut c, &mut m, 0x08, 3, 5);
        assert_eq!(m.rw(0xa2fe), 0xabcd);
    }

    #[test]
    fn add_hl_bc() {
        add_hl_rr!(b, c, 0x09);
    }

    #[test]
    fn add_hl_bc_half_carry() {
        add_hl_rr_half_carry!(b, c, 0x09);
    }

    #[test]
    fn add_hl_bc_carry() {
        add_hl_rr_carry!(b, c, 0x09);
    }

    #[test]
    fn add_hl_bc_only_carry() {
        add_hl_rr_only_carry!(b, c, 0x09);
    }

    #[test]
    fn ld_a_bc() {
        ld_a_rr!(b, c, 0x0a);
    }

    #[test]
    fn dec_bc() {
        dec_rr!(b, c, 0x0b);
    }

    #[test]
    fn dec_bc_half_carry() {
        dec_rr_half_carry!(b, c, 0x0b);
    }

    #[test]
    fn dec_bc_underflow() {
        dec_rr_underflow!(b, c, 0x0b);
    }

    #[test]
    fn inc_c() {
        inc_r!(c, 0x0c);
    }

    #[test]
    fn inc_c_zero() {
        inc_r_zero!(c, 0x0c);
    }

    #[test]
    fn inc_c_half_carry() {
        inc_r_half_carry!(c, 0x0c);
    }

    #[test]
    fn dec_c() {
        dec_r!(c, 0x0d);
    }

    #[test]
    fn dec_c_zero() {
        dec_r_zero!(c, 0x0d);
    }

    #[test]
    fn dec_c_half_carry() {
        dec_r_half_carry!(c, 0x0d);
    }

    #[test]
    fn ld_c_n() {
        ld_r_n!(c, 0x0e);
    }

    #[test]
    fn rrca() {
        let (mut c, mut m) = init();
        c.r.a = 0x7e;
        op(&mut c, &mut m, 0x0f, 1, 1);
        assert_eq!(c.r.a, 0x3f);
    }

    #[test]
    fn rrca_carry() {
        let (mut c, mut m) = init();
        c.r.a = 0xd1;
        op(&mut c, &mut m, 0x0f, 1, 1);
        assert_eq!(c.r.a, 0xe8);
        assert_eq!(c.r.f, C);
    }

    #[test]
    fn rrca_flags() {
        let (mut c, mut m) = init();
        c.r.a = 0x7e;
        c.r.f = Z | N | H | C;
        op(&mut c, &mut m, 0x0f, 1, 1);
        assert_eq!(c.r.a, 0x3f);
        assert_eq!(c.r.f, 0x00);
    }

    #[test]
    fn stop() {
        let (mut c, mut m) = init();
        op(&mut c, &mut m, 0x10, 2, 1);
    }

    #[test]
    fn ld_de_nn() {
        ld_rr_nn!(d, e, 0x11);
    }

    #[test]
    fn ld_de_a() {
        ld_rr_r!(d, e, a, 0x12);
    }

    #[test]
    fn inc_de() {
        inc_rr!(d, e, 0x13);
    }

    #[test]
    fn inc_de_half_carry() {
        inc_rr_half_carry!(d, e, 0x13);
    }

    #[test]
    fn inc_de_overflow() {
        inc_rr_overflow!(d, e, 0x13);
    }

    #[test]
    fn inc_d() {
        inc_r!(d, 0x14);
    }

    #[test]
    fn inc_d_zero() {
        inc_r_zero!(d, 0x14);
    }

    #[test]
    fn inc_d_half_carry() {
        inc_r_half_carry!(d, 0x14);
    }

    #[test]
    fn dec_d() {
        dec_r!(d, 0x15);
    }

    #[test]
    fn dec_d_zero() {
        dec_r_zero!(d, 0x15);
    }

    #[test]
    fn dec_d_half_carry() {
        dec_r_half_carry!(d, 0x15);
    }

    #[test]
    fn ld_d_n() {
        ld_r_n!(d, 0x16);
    }

    #[test]
    fn rla() {
        let (mut c, mut m) = init();
        c.r.a = 0b01010101;
        op(&mut c, &mut m, 0x17, 1, 1);
        assert_eq!(c.r.a, 0b10101010);
        assert_eq!(c.r.f, 0x00);
    }

    #[test]
    fn rla_into_carry() {
        let (mut c, mut m) = init();
        c.r.a = 0b11010101;
        op(&mut c, &mut m, 0x17, 1, 1);
        assert_eq!(c.r.a, 0b10101010);
        assert_eq!(c.r.f, C);
    }

    #[test]
    fn rla_from_carry() {
        let (mut c, mut m) = init();
        c.r.a = 0b01010101;
        c.r.f = C;
        op(&mut c, &mut m, 0x17, 1, 1);
        assert_eq!(c.r.a, 0b10101011);
        assert_eq!(c.r.f, 0x00);
    }

    #[test]
    fn rla_through_carry() {
        let (mut c, mut m) = init();
        c.r.a = 0b11010101;
        c.r.f = C;
        op(&mut c, &mut m, 0x17, 1, 1);
        assert_eq!(c.r.a, 0b10101011);
        assert_eq!(c.r.f, C);
    }

    #[test]
    fn rla_flags() {
        let (mut c, mut m) = init();
        c.r.a = 0b01010101;
        c.r.f = Z | N | H | C;
        op(&mut c, &mut m, 0x17, 1, 1);
        assert_eq!(c.r.a, 0b10101011);
        assert_eq!(c.r.f, 0x00);
    }

    #[test]
    fn jr_e() {
        jr_e!(0x00, 0x18);
    }

    #[test]
    fn jr_e_back() {
        jr_e_back!(0x00, 0x18);
    }

    #[test]
    fn add_hl_de() {
        add_hl_rr!(d, e, 0x19);
    }

    #[test]
    fn add_hl_de_carry() {
        add_hl_rr_carry!(d, e, 0x19);
    }

    #[test]
    fn add_hl_de_half_carry() {
        add_hl_rr_half_carry!(d, e, 0x19);
    }

    #[test]
    fn add_hl_de_only_carry() {
        add_hl_rr_only_carry!(d, e, 0x19);
    }

    #[test]
    fn ld_a_de() {
        ld_a_rr!(d, e, 0x1a);
    }

    #[test]
    fn dec_de() {
        dec_rr!(d, e, 0x1b);
    }

    #[test]
    fn dec_de_half_carry() {
        dec_rr_half_carry!(d, e, 0x1b);
    }

    #[test]
    fn dec_de_underflow() {
        dec_rr_underflow!(d, e, 0x1b);
    }

    #[test]
    fn inc_e() {
        inc_r!(e, 0x1c);
    }

    #[test]
    fn inc_e_half_carry() {
        inc_r_half_carry!(e, 0x1c);
    }

    #[test]
    fn inc_e_zero() {
        inc_r_zero!(e, 0x1c);
    }

    #[test]
    fn dec_e() {
        dec_r!(e, 0x1d);
    }

    #[test]
    fn dec_e_half_carry() {
        dec_r_half_carry!(e, 0x1d);
    }

    #[test]
    fn dec_e_underflow() {
        dec_r_half_carry!(e, 0x1d);
    }

    #[test]
    fn ld_e_n() {
        ld_r_n!(e, 0x1e);
    }

    #[test]
    fn rra() {
        let (mut c, mut m) = init();
        c.r.a = 0b10101010;
        op(&mut c, &mut m, 0x1f, 1, 1);
        assert_eq!(c.r.a, 0b01010101);
        assert_eq!(c.r.f, 0x00);
    }

    #[test]
    fn rra_into_carry() {
        let (mut c, mut m) = init();
        c.r.a = 0b11010101;
        op(&mut c, &mut m, 0x1f, 1, 1);
        assert_eq!(c.r.a, 0b01101010);
        assert_eq!(c.r.f, C);
    }

    #[test]
    fn rra_from_carry() {
        let (mut c, mut m) = init();
        c.r.a = 0b01010100;
        c.r.f = C;
        op(&mut c, &mut m, 0x1f, 1, 1);
        assert_eq!(c.r.a, 0b10101010);
        assert_eq!(c.r.f, 0x00);
    }

    #[test]
    fn rra_through_carry() {
        let (mut c, mut m) = init();
        c.r.a = 0b11010101;
        c.r.f = C;
        op(&mut c, &mut m, 0x1f, 1, 1);
        assert_eq!(c.r.a, 0b11101010);
        assert_eq!(c.r.f, C);
    }

    #[test]
    fn rra_flags() {
        let (mut c, mut m) = init();
        c.r.a = 0b01010100;
        c.r.f = Z | N | H | C;
        op(&mut c, &mut m, 0x1f, 1, 1);
        assert_eq!(c.r.a, 0b10101010);
        assert_eq!(c.r.f, 0x00);
    }

    #[test]
    fn jr_nz_e() {
        jr_e!(0x00, 0x20);
    }

    #[test]
    fn jr_nz_e_back() {
        jr_e_back!(0x00, 0x20);
    }

    #[test]
    fn jr_nz_e_no_jump() {
        jr_e_no_jump!(Z, 0x20);
    }

    #[test]
    fn ld_hl_nn() {
        ld_rr_nn!(h, l, 0x21);
    }

    #[test]
    fn ldi_hl_a() {
        let (mut c, mut m) = init();
        c.r.a = 0xab;
        c.r.h = 0xd2;
        c.r.l = 0xff;
        op(&mut c, &mut m, 0x22, 1, 2);
        assert_eq!(m.rb(0xd2ff), 0xab);
        assert_eq!(c.r.h, 0xd3);
        assert_eq!(c.r.l, 0x00);
    }

    #[test]
    fn inc_hl() {
        inc_rr!(h, l, 0x23);
    }

    #[test]
    fn inc_hl_half_carry() {
        inc_rr_half_carry!(h, l, 0x23);
    }

    #[test]
    fn inc_hl_overflow() {
        inc_rr_overflow!(h, l, 0x23);
    }

    #[test]
    fn inc_h() {
        inc_r!(h, 0x24);
    }

    #[test]
    fn inc_h_half_carry() {
        inc_r_half_carry!(h, 0x24);
    }

    #[test]
    fn inc_h_zero() {
        inc_r_zero!(h, 0x24);
    }

    #[test]
    fn dec_h() {
        dec_r!(h, 0x25);
    }

    #[test]
    fn dec_h_half_carry() {
        dec_r_half_carry!(h, 0x25);
    }

    #[test]
    fn dec_h_zero() {
        dec_r_zero!(h, 0x25);
    }

    #[test]
    fn ld_h_n() {
        ld_r_n!(h, 0x26);
    }

    #[test]
    fn jr_z_e() {
        jr_e!(Z, 0x28);
    }

    #[test]
    fn jr_z_e_back() {
        jr_e_back!(Z, 0x28);
    }

    #[test]
    fn jr_z_e_no_jump() {
        jr_e_no_jump!(0x00, 0x28);
    }

    fn add_hl_hl_helper(h: u8, l: u8, expected_h: u8, expected_l: u8, expected_f: u8) {
        let (mut c, mut m) = init();
        c.r.h = h;
        c.r.l = l;
        op(&mut c, &mut m, 0x29, 1, 2);
        assert_eq!(c.r.h, expected_h);
        assert_eq!(c.r.l, expected_l);
        assert_eq!(c.r.f, expected_f);
    }

    #[test]
    fn add_hl_hl() {
        add_hl_hl_helper(0x12, 0x34, 0x24, 0x68, 0x0);
    }

    #[test]
    fn add_hl_hl_carry() {
        add_hl_hl_helper(0x88, 0x88, 0x11, 0x10, H | C);
    }

    #[test]
    fn add_hl_hl_half_carry() {
        add_hl_hl_helper(0x18, 0x00, 0x30, 0x00, H);
    }

    #[test]
    fn add_hl_hl_only_carry() {
        add_hl_hl_helper(0x82, 0x00, 0x04, 0x00, C);
    }

    #[test]
    fn ld_sp_nn() {
        let (mut c, mut m) = init();
        m.ww(c.r.pc + 1, 0x1234);
        op(&mut c, &mut m, 0x31, 3, 3);
        assert_eq!(c.r.sp, 0x1234);
    }

    #[test]
    fn ldd_hl_a() {
        let (mut c, mut m) = init();
        c.r.a = 0xab;
        c.r.h = 0xd2;
        c.r.l = 0xff;
        op(&mut c, &mut m, 0x32, 1, 2);
        assert_eq!(m.rb(0xd2ff), 0xab);
        assert_eq!(c.r.h, 0xd2);
        assert_eq!(c.r.l, 0xfe);
    }

    #[test]
    fn inc_sp() {
        let (mut c, mut m) = init();
        c.r.sp = 0x1234;
        op(&mut c, &mut m, 0x33, 1, 2);
        assert_eq!(c.r.sp, 0x1235);
    }

    #[test]
    fn inc_at_hl() {
        let (mut c, mut m) = init();
        c.r.h = 0xd0;
        c.r.l = 0x00;
        m.wb(0xd000, 0x12);
        op(&mut c, &mut m, 0x34, 1, 3);
        assert_eq!(m.rb(0xd000), 0x13);
    }

    #[test]
    fn dec_at_hl() {
        let (mut c, mut m) = init();
        c.r.h = 0xd0;
        c.r.l = 0x00;
        m.wb(0xd000, 0x12);
        op(&mut c, &mut m, 0x35, 1, 3);
        assert_eq!(m.rb(0xd000), 0x11);
    }

    #[test]
    fn ld_hl_n() {
        let (mut c, mut m) = init();
        c.r.h = 0xd0;
        c.r.l = 0x00;
        m.wb(c.r.pc + 1, 0x12);
        op(&mut c, &mut m, 0x36, 2, 3);
        assert_eq!(m.rb(0xd000), 0x12);
    }

    #[test]
    fn scf() {
        let (mut c, mut m) = init();
        op(&mut c, &mut m, 0x37, 1, 1);
        assert_eq!(c.r.f, C);
    }

    #[test]
    fn jr_c_e() {
        jr_e!(C, 0x38);
    }

    #[test]
    fn jr_c_e_back() {
        jr_e_back!(C, 0x38);
    }

    #[test]
    fn jr_c_e_no_jump() {
        jr_e_no_jump!(0x00, 0x38);
    }

    fn add_hl_sp_helper(h: u8, l: u8, sp: u16, expected_h: u8, expected_l: u8, expected_f: u8) {
        let (mut c, mut m) = init();
        c.r.h = h;
        c.r.l = l;
        c.r.sp = sp;
        op(&mut c, &mut m, 0x39, 1, 2);
        assert_eq!(c.r.h, expected_h);
        assert_eq!(c.r.l, expected_l);
        assert_eq!(c.r.f, expected_f);
    }

    #[test]
    fn add_hl_sp() {
        add_hl_sp_helper(0x12, 0x34, 0x3451, 0x46, 0x85, 0x0);
    }

    #[test]
    fn add_hl_sp_carry() {
        add_hl_sp_helper(0xff, 0xff, 0x0002, 0x00, 0x01, H | C);
    }

    #[test]
    fn add_hl_sp_half_carry() {
        add_hl_sp_helper(0x18, 0x00, 0x0900, 0x21, 0x00, H);
    }

    #[test]
    fn add_hl_sp_only_carry() {
        add_hl_sp_helper(0x82, 0x00, 0x8000, 0x02, 0x00, C);
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
