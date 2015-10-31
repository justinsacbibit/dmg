use super::mmu::Mmu;
use super::cpu::Cpu;
use super::gpu::Gpu;

// The number of clock cycles it takes to render a single frame
const FRAME_TIME: u32 = 70224;

pub struct Dmg {
    // central processing unit
    cpu: Cpu,

    // memory management unit
    mmu: Mmu,

    // graphics processing unit
    gpu: Gpu,
}

impl Dmg {
    pub fn new(c: Cpu, m: Mmu, g: Gpu) -> Dmg {
        Dmg {
            cpu: c,
            mmu: m,
            gpu: g,
        }
    }

    #[allow(dead_code)]
    pub fn reset(&mut self) {
        // TODO
    }

    pub fn frame(&mut self) {
        let mut frame_clock = self.cpu.t + FRAME_TIME;

        // TODO: Figure out if this runs the correct number of times
        while frame_clock > 0 {
            let m_cycles = self.cpu.exec(&mut self.mmu);
            self.cpu.r.pc &= 65535;
            self.gpu.step(m_cycles);
            frame_clock -= 1;
        }
    }
}
