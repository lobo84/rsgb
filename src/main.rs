use std::any::Any;
use std::borrow::{Borrow, BorrowMut};
use std::error;
use std::fmt;
use std::fs;
use std::io::stdin;
use std::ops::Index;
use std::path::PathBuf;
use std::process::exit;
use std::time::{Duration, Instant};

use clap::{AppSettings, Parser};
use pixels::{Error, Pixels, SurfaceTexture};
use winit::dpi::{LogicalPosition, LogicalSize, PhysicalSize};
use winit::event::{Event, VirtualKeyCode};
use winit::event_loop::{ControlFlow, EventLoop};
use winit_input_helper::WinitInputHelper;

use crate::cpu::Cpu;
use crate::display::pixel::PixelWindow;
use crate::mmu::Memory;
use crate::ppu::Ppu;

mod mmu;
mod cpu;
mod ppu;
mod display;

const SCREEN_WIDTH: u32 = 160;
const SCREEN_HEIGHT: u32 = 144;


fn create_window(
    title: &str,
    event_loop: &EventLoop<()>,
) -> (winit::window::Window, u32, u32, f64) {
    let window = winit::window::WindowBuilder::new()
        .with_visible(false)
        .with_title(title)
        .build(event_loop)
        .unwrap();
    let hidpi_factor = window.scale_factor();

    // Get dimensions
    let width = SCREEN_WIDTH as f64;
    let height = SCREEN_HEIGHT as f64;
    let (monitor_width, monitor_height) = {
        if let Some(monitor) = window.current_monitor() {
            let size = monitor.size().to_logical(hidpi_factor);
            (size.width, size.height)
        } else {
            (width, height)
        }
    };
    let scale = (monitor_height / height * 2.0 / 3.0).round().max(1.0);

    // Resize, center, and display the window
    let min_size: winit::dpi::LogicalSize<f64> =
        PhysicalSize::new(width, height).to_logical(hidpi_factor);
    let default_size = LogicalSize::new(width * scale, height * scale);
    let center = LogicalPosition::new(
        (monitor_width - width * scale) / 2.0,
        (monitor_height - height * scale) / 2.0,
    );
    window.set_inner_size(default_size);
    window.set_min_inner_size(Some(min_size));
    window.set_outer_position(center);
    window.set_visible(true);

    let size = default_size.to_physical::<f64>(hidpi_factor);
    (
        window,
        size.width.round() as u32,
        size.height.round() as u32,
        hidpi_factor,
    )
}

#[derive(Parser, Debug)]
#[clap(about, version, author)]
struct Args {
    #[clap(short, long)]
    step_cpu: bool,
    #[clap(short, long, default_value_t = 0)]
    break_at: u16,
    #[clap(parse(from_os_str))]
    boot_rom: PathBuf,
    #[clap(parse(from_os_str))]
    cartridge_rom: PathBuf,
}

fn main() -> Result<(), Box<dyn error::Error + 'static>> {
    let args = Args::parse();

    let event_loop = EventLoop::new();
    let mut input = WinitInputHelper::new();

    let (window, p_width, p_height, mut _hidpi_factor) =
        create_window("rsgb", &event_loop);

    let surface_texture = SurfaceTexture::new(p_width, p_height, window.borrow());
    let mut display = PixelWindow::new();
    let mut pixels = Pixels::new(SCREEN_WIDTH, SCREEN_HEIGHT, surface_texture)?;
    let bootrom = fs::read(args.boot_rom)?;
    let cart = fs::read(args.cartridge_rom)?;
    let mut cpu = Cpu::new(args.step_cpu, args.break_at);
    let mut ppu = Ppu::default();
    let mut memory = Memory::default();

    memory.load(cart.as_slice());
    memory.load(bootrom.as_slice());
    event_loop.run(move |event, _, control_flow| {
        loop {
            cpu.step(&mut memory);
            let draw = ppu.step(&mut memory, display.borrow_mut());
            if draw {
                window.request_redraw();
                break;
            }
        }

        if let Event::RedrawRequested(_) = event {
            display.draw(pixels.get_frame());
            pixels.render();
        }

        if input.update(&event) {
            if input.key_pressed(VirtualKeyCode::Escape) || input.quit() {
                *control_flow = ControlFlow::Exit;
                return;
            }
            // Adjust high DPI factor
            if let Some(factor) = input.scale_factor_changed() {
                _hidpi_factor = factor;
            }
            // Resize the window
            if let Some(size) = input.window_resized() {
                pixels.resize_surface(size.width, size.height);
            }
        }
    });

}


