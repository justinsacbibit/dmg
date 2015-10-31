#[macro_use]
extern crate glium;

use std::env;
use std::fs::File;
use std::io::Read;

mod cpu;
mod dmg;
mod gl;
mod gpu;
mod mmu;
mod registers;

fn main() {
    let mut args = env::args();
    let path = args.nth(1).unwrap();
    let mut rom = Vec::new();
    let file = File::open(&path);
    match file.and_then(|mut f| f.read_to_end(&mut rom)) {
        Ok(..) => {}
        Err(e) => {
            println!("failed to read {}: {}", path, e);
            return
        }
    }

    let cpu = cpu::Cpu::new();

    let mut mmu = mmu::Mmu::new();
    mmu.load_cartridge(rom);

    let gpu = gpu::Gpu::new();

    let dmg = dmg::Dmg::new(cpu, mmu, gpu);

    gl::Gl::run(dmg);
}
