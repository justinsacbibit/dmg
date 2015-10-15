mod cpu;
mod mmu;
mod registers;

#[allow(dead_code)]
fn main() {
    let mut cpu = cpu::Cpu::new();
    cpu.reset();

    let mut mm = mmu::Mmu::new();
    println!("{}", cpu.exec(&mut mm));
    println!("Hello, world!");
}
