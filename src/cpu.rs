pub struct Cpu {
    // Time clock
    m: u32,

    // Registers
    a: u8
    b: u8
    c: u8
    d: u8
    e: u8
    h: u8
    l: u8
    f: u8
    pc: u16,
    sp: u16,
    // Clock for last instr
    rm: u32,
}
