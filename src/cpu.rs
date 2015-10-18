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
        macro_rules! ld_r_r {
            ($dest: ident, $src: ident) => ({
                self.r.$dest = self.r.$src;
                1
            })
        }
        macro_rules! ld_r_hl {
            ($r: ident) => ({
                self.r.$r = m.rb(self.r.hl());
                2
            })
        }
        macro_rules! ld_hl_r {
            ($r: ident) => ({
                m.wb(self.r.hl(), self.r.$r);
                2
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
                self.r.f &= Z | C;
                self.r.f |= N | H;
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
            0x3a => { self.r.a = m.rb(self.r.hl()); dec_ss!(h, l) } // LDD A, (HL)
            0x3b => { self.r.sp -= 1; 2 }, // DEC SP
            0x3c => inc!(a), // INC A
            0x3d => dec!(a), // DEC A
            0x3e => ld_n!(a), // LD A, n
            0x3f => { // CCF
                self.r.f &= !(N | H);
                if self.r.f & C == C {
                    self.r.f &= !C
                } else {
                    self.r.f |= C
                }
                1
            }
            0x40 => ld_r_r!(b, b), // LD B, B
            0x41 => ld_r_r!(b, c), // LD B, C
            0x42 => ld_r_r!(b, d), // LD B, D
            0x43 => ld_r_r!(b, e), // LD B, E
            0x44 => ld_r_r!(b, h), // LD B, H
            0x45 => ld_r_r!(b, l), // LD B, L
            0x46 => ld_r_hl!(b), // LD B, (HL)
            0x47 => ld_r_r!(b, a), // LD B, A
            0x48 => ld_r_r!(c, b), // LD C, B
            0x49 => ld_r_r!(c, c), // LD C, C
            0x4a => ld_r_r!(c, d), // LD C, D
            0x4b => ld_r_r!(c, e), // LD C, E
            0x4c => ld_r_r!(c, h), // LD C, H
            0x4d => ld_r_r!(c, l), // LD C, L
            0x4e => ld_r_hl!(c), // LD C, (HL)
            0x4f => ld_r_r!(c, a), // LD C, A
            0x50 => ld_r_r!(d, b), // LD D, B
            0x51 => ld_r_r!(d, c), // LD D, C
            0x52 => ld_r_r!(d, d), // LD D, D
            0x53 => ld_r_r!(d, e), // LD D, E
            0x54 => ld_r_r!(d, h), // LD D, H
            0x55 => ld_r_r!(d, l), // LD D, L
            0x56 => ld_r_hl!(d), // LD D, (HL)
            0x57 => ld_r_r!(d, a), // LD D, A
            0x58 => ld_r_r!(e, b), // LD E, B
            0x59 => ld_r_r!(e, c), // LD E, C
            0x5a => ld_r_r!(e, d), // LD E, D
            0x5b => ld_r_r!(e, e), // LD E, E
            0x5c => ld_r_r!(e, h), // LD E, H
            0x5d => ld_r_r!(e, l), // LD E, L
            0x5e => ld_r_hl!(e), // LD E, (HL)
            0x5f => ld_r_r!(e, a), // LD E, A
            0x60 => ld_r_r!(h, b), // LD H, B
            0x61 => ld_r_r!(h, c), // LD H, C
            0x62 => ld_r_r!(h, d), // LD H, D
            0x63 => ld_r_r!(h, e), // LD H, E
            0x64 => ld_r_r!(h, h), // LD H, H
            0x65 => ld_r_r!(h, l), // LD H, L
            0x66 => ld_r_hl!(h), // LD H, (HL)
            0x67 => ld_r_r!(h, a), // LD H, A
            0x68 => ld_r_r!(l, b), // LD L, B
            0x69 => ld_r_r!(l, c), // LD L, C
            0x6a => ld_r_r!(l, d), // LD L, D
            0x6b => ld_r_r!(l, e), // LD L, E
            0x6c => ld_r_r!(l, h), // LD L, H
            0x6d => ld_r_r!(l, l), // LD L, L
            0x6e => ld_r_hl!(l), // LD L, (HL)
            0x6f => ld_r_r!(l, a), // LD L, A
            0x70 => ld_hl_r!(b), // LD (HL), B
            0x71 => ld_hl_r!(c), // LD (HL), C
            0x72 => ld_hl_r!(d), // LD (HL), D
            0x73 => ld_hl_r!(e), // LD (HL), E
            0x74 => ld_hl_r!(h), // LD (HL), H
            0x75 => ld_hl_r!(l), // LD (HL), L
            0x76 => { 1 } // TODO: HALT
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
    fn ldi_a_hl() {
        let (mut c, mut m) = init();
        c.r.h = 0xc0;
        c.r.l = 0xff;
        m.wb(0xc0ff, 0x12);
        op(&mut c, &mut m, 0x2a, 1, 2);
        assert_eq!(c.r.a, 0x12);
        assert_eq!(c.r.h, 0xc1);
        assert_eq!(c.r.l, 0x00);
    }

    #[test]
    fn dec_hl() {
        dec_rr!(h, l, 0x2b);
    }

    #[test]
    fn dec_hl_half_carry() {
        dec_rr_half_carry!(h, l, 0x2b);
    }

    #[test]
    fn dec_hl_underflow() {
        dec_rr_underflow!(h, l, 0x2b);
    }

    #[test]
    fn inc_l() {
        inc_r!(l, 0x2c);
    }

    #[test]
    fn inc_l_half_carry() {
        inc_r_half_carry!(l, 0x2c);
    }

    #[test]
    fn inc_l_zero() {
        inc_r_zero!(l, 0x2c);
    }

    #[test]
    fn dec_l() {
        dec_r!(l, 0x2d);
    }

    #[test]
    fn dec_l_half_carry() {
        dec_r_half_carry!(l, 0x2d);
    }

    #[test]
    fn dec_l_underflow() {
        dec_r_half_carry!(l, 0x2d);
    }

    #[test]
    fn ld_l_n() {
        ld_r_n!(l, 0x2e);
    }

    #[test]
    fn cpl() {
        let (mut c, mut m) = init();
        c.r.a = 0b01101010;
        c.r.f = Z | C;
        op(&mut c, &mut m, 0x2f, 1, 1);
        assert_eq!(c.r.a, 0b10010101);
        assert_eq!(c.r.f, Z | N | H | C);
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

    #[test]
    fn ldd_a_hl() {
        let (mut c, mut m) = init();
        c.r.h = 0xd0;
        c.r.l = 0x00;
        m.wb(0xd000, 0x12);
        op(&mut c, &mut m, 0x3a, 1, 2);
        assert_eq!(c.r.a, 0x12);
        assert_eq!(c.r.h, 0xcf);
        assert_eq!(c.r.l, 0xff);
    }

    #[test]
    fn dec_sp() {
        let (mut c, mut m) = init();
        c.r.sp = 0x1234;
        op(&mut c, &mut m, 0x3b, 1, 2);
        assert_eq!(c.r.sp, 0x1233);
        assert_eq!(c.r.f, 0x00);
    }

    #[test]
    fn inc_a() {
        inc_r!(a, 0x3c);
    }

    #[test]
    fn inc_a_half_carry() {
        inc_r_half_carry!(a, 0x3c);
    }

    #[test]
    fn inc_a_zero() {
        inc_r_zero!(a, 0x3c);
    }

    #[test]
    fn dec_a() {
        dec_r!(a, 0x3d);
    }

    #[test]
    fn dec_a_half_carry() {
        dec_r_half_carry!(a, 0x3d);
    }

    #[test]
    fn dec_a_underflow() {
        dec_r_half_carry!(a, 0x3d);
    }

    #[test]
    fn ld_a_n() {
        ld_r_n!(a, 0x3e);
    }

    macro_rules! ccf {
        ($c: expr, $expected_c: expr) => ({
            let (mut c, mut m) = init();
            c.r.f = Z | N | H | $c;
            op(&mut c, &mut m, 0x3f, 1, 1);
            assert_eq!(c.r.f, Z | $expected_c);
        })
    }

    #[test]
    fn ccf() {
        ccf!(0x00, C);
    }

    #[test]
    fn ccf_reset() {
        ccf!(C, 0x00);
    }

    macro_rules! ld_r_r {
        ($dest: ident, $src: ident, $op: expr) => ({
            let (mut c, mut m) = init();
            c.r.$src = 0x12;
            op(&mut c, &mut m, $op, 1, 1);
            assert_eq!(c.r.$dest, 0x12);
        })
    }

    macro_rules! ld_r_hl {
        ($r: ident, $op: expr) => ({
            let (mut c, mut m) = init();
            c.r.h = 0xd0;
            c.r.l = 0x00;
            m.wb(0xd000, 0x12);
            op(&mut c, &mut m, $op, 1, 2);
            assert_eq!(c.r.$r, 0x12);
        })
    }

    #[test]
    fn ld_b_b() {
        ld_r_r!(b, b, 0x40);
    }

    #[test]
    fn ld_b_c() {
        ld_r_r!(b, c, 0x41);
    }

    #[test]
    fn ld_b_d() {
        ld_r_r!(b, d, 0x42);
    }

    #[test]
    fn ld_b_e() {
        ld_r_r!(b, e, 0x43);
    }

    #[test]
    fn ld_b_h() {
        ld_r_r!(b, h, 0x44);
    }

    #[test]
    fn ld_b_l() {
        ld_r_r!(b, l, 0x45);
    }

    #[test]
    fn ld_b_hl() {
        ld_r_hl!(b, 0x46);
    }

    #[test]
    fn ld_b_a() {
        ld_r_r!(b, a, 0x47);
    }

    #[test]
    fn ld_c_b() {
        ld_r_r!(c, b, 0x48);
    }

    #[test]
    fn ld_c_c() {
        ld_r_r!(c, c, 0x49);
    }

    #[test]
    fn ld_c_d() {
        ld_r_r!(c, d, 0x4a);
    }

    #[test]
    fn ld_c_e() {
        ld_r_r!(c, e, 0x4b);
    }

    #[test]
    fn ld_c_h() {
        ld_r_r!(c, h, 0x4c);
    }

    #[test]
    fn ld_c_l() {
        ld_r_r!(c, l, 0x4d);
    }

    #[test]
    fn ld_c_hl() {
        ld_r_hl!(c, 0x4e);
    }

    #[test]
    fn ld_c_a() {
        ld_r_r!(c, a, 0x4f);
    }

    #[test]
    fn ld_d_b() {
        ld_r_r!(d, b, 0x50);
    }

    #[test]
    fn ld_d_c() {
        ld_r_r!(d, c, 0x51);
    }

    #[test]
    fn ld_d_d() {
        ld_r_r!(d, d, 0x52);
    }

    #[test]
    fn ld_d_e() {
        ld_r_r!(d, e, 0x53);
    }

    #[test]
    fn ld_d_h() {
        ld_r_r!(d, h, 0x54);
    }

    #[test]
    fn ld_d_l() {
        ld_r_r!(d, l, 0x55);
    }

    #[test]
    fn ld_d_hl() {
        ld_r_hl!(d, 0x56);
    }

    #[test]
    fn ld_d_a() {
        ld_r_r!(d, a, 0x57);
    }

    #[test]
    fn ld_e_b() {
        ld_r_r!(e, b, 0x58);
    }

    #[test]
    fn ld_e_c() {
        ld_r_r!(e, c, 0x59);
    }

    #[test]
    fn ld_e_d() {
        ld_r_r!(e, d, 0x5a);
    }

    #[test]
    fn ld_e_e() {
        ld_r_r!(e, e, 0x5b);
    }

    #[test]
    fn ld_e_h() {
        ld_r_r!(e, h, 0x5c);
    }

    #[test]
    fn ld_e_l() {
        ld_r_r!(e, l, 0x5d);
    }

    #[test]
    fn ld_e_hl() {
        ld_r_hl!(e, 0x5e);
    }

    #[test]
    fn ld_e_a() {
        ld_r_r!(e, a, 0x5f);
    }

    #[test]
    fn ld_h_b() {
        ld_r_r!(h, b, 0x60);
    }

    #[test]
    fn ld_h_c() {
        ld_r_r!(h, c, 0x61);
    }

    #[test]
    fn ld_h_d() {
        ld_r_r!(h, d, 0x62);
    }

    #[test]
    fn ld_h_e() {
        ld_r_r!(h, e, 0x63);
    }

    #[test]
    fn ld_h_h() {
        ld_r_r!(h, h, 0x64);
    }

    #[test]
    fn ld_h_l() {
        ld_r_r!(h, l, 0x65);
    }

    #[test]
    fn ld_h_hl() {
        ld_r_hl!(h, 0x66);
    }

    #[test]
    fn ld_h_a() {
        ld_r_r!(h, a, 0x67);
    }

    #[test]
    fn ld_l_b() {
        ld_r_r!(l, b, 0x68);
    }

    #[test]
    fn ld_l_c() {
        ld_r_r!(l, c, 0x69);
    }

    #[test]
    fn ld_l_d() {
        ld_r_r!(l, d, 0x6a);
    }

    #[test]
    fn ld_l_e() {
        ld_r_r!(l, e, 0x6b);
    }

    #[test]
    fn ld_l_h() {
        ld_r_r!(l, h, 0x6c);
    }

    #[test]
    fn ld_l_l() {
        ld_r_r!(l, l, 0x6d);
    }

    #[test]
    fn ld_l_hl() {
        ld_r_hl!(l, 0x6e);
    }

    #[test]
    fn ld_l_a() {
        ld_r_r!(l, a, 0x6f);
    }

    macro_rules! ld_hl_r {
        ($r: ident, $op: expr) => ({
            let (mut c, mut m) = init();
            c.r.h = 0xd0;
            c.r.l = 0x00;
            c.r.$r = 0x12;
            op(&mut c, &mut m, $op, 1, 2);
            assert_eq!(m.rb(0xd000), 0x12);
        })
    }

    #[test]
    fn ld_hl_b() {
        ld_hl_r!(b, 0x70);
    }

    #[test]
    fn ld_hl_c() {
        ld_hl_r!(c, 0x71);
    }

    #[test]
    fn ld_hl_d() {
        ld_hl_r!(d, 0x72);
    }

    #[test]
    fn ld_hl_e() {
        ld_hl_r!(e, 0x73);
    }

    #[test]
    fn ld_hl_h() {
        let (mut c, mut m) = init();
        c.r.h = 0xd0;
        c.r.l = 0x12;
        op(&mut c, &mut m, 0x74, 1, 2);
        assert_eq!(m.rb(0xd012), 0xd0);
    }

    #[test]
    fn ld_hl_l() {
        let (mut c, mut m) = init();
        c.r.h = 0xd0;
        c.r.l = 0x12;
        op(&mut c, &mut m, 0x75, 1, 2);
        assert_eq!(m.rb(0xd012), 0x12);
    }

    #[test]
    fn halt() {
        // TODO
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
