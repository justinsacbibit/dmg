mod cpu;
mod mmu;

fn main() {
    let mut cpu = cpu::Cpu::new();
    cpu.reset();

    let mut mm = mmu::Mmu::new();
    println!("{} {}", cpu.exec(0x01, &mut mm), cpu.pc);
    println!("Hello, world!");
}
