use std::any::Any;
use std::borrow::{Borrow, BorrowMut};
use std::io::stdin;
use std::fs;
use std::error;
use std::process::exit;
use std::ops::Index;
use std::fmt;



use pixels::{Error, Pixels, SurfaceTexture};
use winit::dpi::{LogicalPosition, LogicalSize, PhysicalSize};
use winit::event::{Event, VirtualKeyCode};
use winit::event_loop::{ControlFlow, EventLoop};
use winit_input_helper::WinitInputHelper;

mod mmu;
mod cpu;
mod ppu;
mod display;

use ppu::Ppu;
use mmu::Memory;
use cpu::Cpu;

use display::Display;
use display::console::Console;
use display::pixel::PixelWindow;
use crate::ppu::State::VBlank;

const SCREEN_WIDTH: u32 = 160;
const SCREEN_HEIGHT: u32 = 144;


fn create_window(
    title: &str,
    event_loop: &EventLoop<()>,
) -> (winit::window::Window, u32, u32, f64) {
    // Create a hidden window so we can estimate a good default window size
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
fn main() -> Result<(), Box<dyn error::Error + 'static>> {
    let event_loop = EventLoop::new();
    let mut input = WinitInputHelper::new();

    let (window, p_width, p_height, mut _hidpi_factor) =
        create_window("Conway's Game of Life", &event_loop);

    let surface_texture = SurfaceTexture::new(p_width, p_height, window.borrow());
    let mut display = PixelWindow::new();
    let mut pixels = Pixels::new(SCREEN_WIDTH, SCREEN_HEIGHT, surface_texture)?;
    let bootrom = fs::read("/Users/niclasbrandt/Downloads/DMG_ROM.bin")?;
    //let bootrom = fs::read("/Users/niclasbrandt/Downloads/sgb_bios.bin")?;
    let mut cpu = cpu::Cpu::default();
    let mut ppu = Ppu::default();
    let mut memory = mmu::Memory::default();
    //let display = Console::default();


    memory.load(bootrom.as_slice());
    //memory.data[0 .. 256].copy_from_slice(bootrom.as_slice());
    //println!("{:x?}", memory.read8(0x002f));
    //println!("{:?}", cpu);

    let mut prev_ppu_state = ppu::State::OamSearch;
    let mut clock = 0u32;
    event_loop.run(move |event, _, control_flow| {
        loop {
            cpu.step(clock, &mut memory);
            let ppu_state = ppu.step(clock, &mut memory, display.borrow_mut());
            clock += 1;
            if matches!(prev_ppu_state, ppu::State::VBlank) && matches!(ppu_state, ppu::State::OamSearch) {
                window.request_redraw();
                //println!("draw");
                break;
            }
            prev_ppu_state = ppu_state;
        }
        //println!("{:?}", ppu_state);

        // The one and only event that winit_input_helper doesn't have for us...
        if let Event::RedrawRequested(_) = event {
            display.draw(pixels.get_frame());
            pixels.render();
        }


        // For everything else, for let winit_input_helper collect events to build its state.
        // It returns `true` when it is time to update our game state and request a redraw.
        if input.update(&event) {
            // Close events
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
            //window.request_redraw();
        }
    });

    // let bootrom = fs::read("/Users/niclasbrandt/Downloads/DMG_ROM.bin")?;
    // let mut cpu = cpu::Cpu::default();
    // let mut ppu = Ppu::default();
    // let mut memory = mmu::Memory::default();
    // //let display = Console::default();
    // let display = PixelWindow::default();
    // memory.load(bootrom.as_slice());
    // //memory.data[0 .. 256].copy_from_slice(bootrom.as_slice());
    // println!("{:x?}", memory.read8(0x002f));
    // println!("{:?}", cpu);
    // let mut input_string = String::new();
    // loop {
    //     //println!(" {:?}", cpu);
    //
    //     //if cpu.read_reg16(PC) >= 0x000c {
    //     //    stdin().read_line(&mut input_string);
    //     //}
    //     cpu.step(&mut memory);
    //     ppu.step(&mut memory, &display);
    // }
}


