extern crate glium;

use std::thread;

use super::dmg::Dmg;

pub struct Gl;

impl Gl {
    pub fn run(mut dmg: Dmg) {
        use glium::DisplayBuild;
        let width = 160;
        let height = 144;
        let ratio = 1 + (width / 10);
        let display = glium::glutin::WindowBuilder::new()
            .with_dimensions(width + 10 * ratio, height + 9 * ratio)
            .build_glium()
            .unwrap();
        loop {
            for ev in display.poll_events() {
                match ev {
                    glium::glutin::Event::Closed => return,
                    _ => ()
                }
            }

            dmg.frame();
            

            thread::sleep_ms(10);
        }
    }
}
