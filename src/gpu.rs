enum Mode {
    HBlank = 0x00,
    VBlank = 0x01,
    RdOam = 0x02,
    RdVram = 0x03,
}

// 160 by 144 pixels, 4 1 byte values per pixel (RGBA)
const FRAME_BUFFER_SIZE: usize = 92160;

pub struct Gpu {
    mode: Mode,
    mode_clock: u32,
    line: u8,

    frame_buffer: [u8; FRAME_BUFFER_SIZE],

    tile_set: [[[u8; 8]; 8]; 384],

    vram: [u8; 8192],
}

impl Gpu {
    pub fn new() -> Gpu {
        Gpu {
            mode: Mode::RdOam,
            mode_clock: 0,
            line: 0,

            frame_buffer: [255; FRAME_BUFFER_SIZE],

            tile_set: [[[0; 8]; 8]; 384],
        }
    }

    pub fn reset(&mut self) {
        self.frame_buffer = [255; FRAME_BUFFER_SIZE];
        // TODO: Put image data
        self.tile_set = [[[0; 8]; 8]; 384];
    }

    fn pixel_position(&self, x: u8, y: u8) -> usize {
        (y * 160 + x) as usize
    }

    fn update_tile()

    pub fn step(&mut self, m_cycles: u32) {
        let clock_cycles = m_cycles * 4;
        self.mode_clock += clock_cycles;
        match self.mode {
            Mode::RdOam => {
                if self.mode_clock >= 80 {
                    self.mode_clock = 0;
                    self.mode = Mode::RdVram;
                }
            }
            Mode::RdVram => {
                if self.mode_clock >= 172 {
                    self.mode_clock = 0;
                    self.mode = Mode::HBlank;

                    self.render_scan();
                }
            }
            Mode::HBlank => {
                if self.mode_clock >= 204 {
                    self.mode_clock = 0;
                    self.line += 1;

                    if self.line == 143 {
                        self.mode = Mode::VBlank;
                        // TODO: put image data
                    } else {
                        self.mode = Mode::RdOam;
                    }
                }
            }
            Mode::VBlank => {
                if self.mode_clock >= 456 {
                    self.mode_clock = 0;
                    self.line += 1;
                    if self.line > 153 {
                        self.mode = Mode::HBlank;
                        self.line += 1;
                    }
                    if self.line > 153 {
                        self.mode = Mode::RdOam;
                        self.line = 0;
                    }
                }
            }
        }
    }

    // TODO
    fn render_scan(&mut self) {
        
    }
}
