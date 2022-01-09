use crate::display::Display;

pub struct Console {
    palette: [char; 4]
}

impl Console {
    pub fn default() -> Self {
        Self {
            palette: ['█', '▒', '░', ' '],
        }
    }
}

impl Display for Console {
    fn write(&mut self, color: u8) {
        print!("{}", self.palette[color as usize]);
    }

    fn h_blank(&mut self) {
        print!("\n");
    }

    fn v_blank(&mut self) {
        print!("\n");
        //print!("\n == vblank == \n");
        //let mut input_string = String::new();
        //stdin().read_line(&mut input_string);
    }
}
