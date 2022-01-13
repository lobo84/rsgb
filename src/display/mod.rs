pub mod pixel;

pub trait Display {
    fn write(&mut self, color: u8);
    fn h_blank(&mut self);
    fn v_blank(&mut self);
}
