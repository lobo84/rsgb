use crate::display::Display;

const WIDTH: usize = 160;
const HEIGHT: usize = 144;
const SIZE: usize = WIDTH * HEIGHT * 4;

pub struct PixelWindow {
    palette: [[u8; 4]; 4],
    screen: [u8; SIZE],
    x: usize,
    y: usize,
}

impl PixelWindow {
    pub fn new() -> Self {
        Self {
            palette: [[0xff, 0x00, 0x00, 0x00],
                      [0x00, 0xff, 0x00, 0xff],
                      [0x00, 0xff, 0xff, 0xff],
                      [0xff, 0xff, 0x00, 0xff]],
            screen: [0; SIZE],
            x: 0,
            y: 0,
        }
    }

    pub fn draw(&self, screen: &mut [u8]) {
        screen.copy_from_slice(self.screen.as_slice());
    }

}

impl Display for PixelWindow {
    fn write(&mut self, value: u8) {
        let i = self.y*WIDTH*4 + self.x*4;
        let color = self.palette[value as usize];
        self.screen[i] = color[0];
        self.screen[i+1] = color[1];
        self.screen[i+2] = color[2];
        self.screen[i+3] = color[3];
        self.x += 1;
    }

    fn h_blank(&mut self) {
        self.y += 1;
        self.x = 0;
    }

    fn v_blank(&mut self) {
        self.y = 0;
        self.x = 0;
    }
}

