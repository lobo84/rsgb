use std::collections::VecDeque;

use PpuRegister::*;

use crate::display::Display;
use crate::mmu::Memory;

pub struct Ppu {
    fetcher: Fetcher,
    state: State,
    ticks: u32,
    x: u8,
}

#[derive(Debug, Copy, Clone)]
enum PpuRegister {
    Ly, Scy
}

#[derive(Debug, Copy, Clone)]
pub enum State { OamSearch, PixelTransfer, HBlank, VBlank }

enum FetcherState {
    ReadTileID,
    ReadTileData0,
    ReadTileData1,
    PushToFIFO,
}

struct Fetcher {
    fifo: VecDeque<u8>,
    ticks: u32,
    state: FetcherState,
    tile_index: u8,
    tile_id: u8,
    tile_line: u8,
    map_addr: u16,
    pixeldata: u8,
    tile_data: [u8; 8],
}

impl Fetcher {

    fn start(&mut self, map_addr: u16, tile_line: u8) {
        self.tile_index = 0;
        self.map_addr = map_addr;
        self.tile_line = tile_line;
        self.state = FetcherState::ReadTileID;
        self.fifo.clear();
        self.pixeldata = 0;
    }

    fn step(&mut self, memory: &Memory) {
        self.ticks += 2;
        if self.ticks < 2 {
            return;
        }
        self.ticks = 0;
        match self.state {
            FetcherState::ReadTileID => {
                self.tile_id = memory.read8(self.map_addr + self.tile_index as u16);
                self.state = FetcherState::ReadTileData0;
            },
            FetcherState::ReadTileData0 => {
                self.read_tile_data(memory, 0);
                self.state = FetcherState::ReadTileData1;
            },
            FetcherState::ReadTileData1 => {
                self.read_tile_data(memory, 1);
                self.state = FetcherState::PushToFIFO;
            }
            FetcherState::PushToFIFO => {
                if self.fifo.len() <= 8 {
                    for i in (0..8).rev() {
                        self.fifo.push_front(self.tile_data[i]);
                    }
                    self.tile_index += 1;
                    self.state = FetcherState::ReadTileID;
                }
            }
        }
    }

    fn read_tile_data(&mut self, memory: &Memory, bit_plane: u16) {
        let offset = 0x8000 + self.tile_id as u16 * 16;
        let addr = offset + self.tile_line as u16 * 2;
        let data = memory.read8(addr + bit_plane);
        for bitpos in 0..8 {
            if bit_plane == 0 {
                // Least significant bit, replace the previous value.
                self.tile_data[bitpos] = (data >> bitpos) & 1
            } else {
                // Most significant bit, update the previous value.
                self.tile_data[bitpos] |= ((data >> bitpos) & 1) << 1
            }
        }
    }
}

impl Default for Ppu {
    fn default() -> Self {
        Self {
            fetcher: Fetcher {
                fifo: Default::default(),
                ticks: 0,
                state: FetcherState::ReadTileID,
                tile_index: 0,
                tile_id: 0,
                tile_line: 0,
                map_addr: 0,
                pixeldata: 0,
                tile_data: [0; 8],
            },
            state: State::OamSearch,
            ticks: 0,
            x: 0,
        }
    }
}

impl Ppu {
    fn reg_addr(reg: PpuRegister) -> u16 {
        match reg {
            PpuRegister::Ly => 0xff44,
            PpuRegister::Scy => 0xff42,
        }
    }

    fn write_reg8(&self, reg: PpuRegister, memory: &mut Memory, value: u8) {
        memory.write8(Ppu::reg_addr(reg), value);
    }

    fn inc_reg(&self, inc: u8, memory: &mut Memory, reg: PpuRegister) {
        let val = self.read_reg8(reg, memory);
        self.write_reg8(reg, memory, val + inc);
    }

    fn read_reg8(&self, reg: PpuRegister, memory: &mut Memory) -> u8 {
        memory.read8(Ppu::reg_addr(reg))
    }

    pub fn step(&mut self, memory: &mut Memory, display: &mut dyn Display) -> bool {
        self.ticks += 1;
        match self.state {
            State::OamSearch => {
                if self.ticks == 40 {
                    self.x = 0;
                    let scy = self.read_reg8(Scy, memory);
                    let ly = self.read_reg8(Ly, memory) as u16 + scy as u16;
                    let tile_line = (ly % 8) as u8;
                    let tile_map_row_addr = 0x9800 + (ly / 8) as u16 * 32;
                    self.fetcher.start(tile_map_row_addr, tile_line);
                    self.state = State::PixelTransfer;
                }
            },
            State::PixelTransfer => {
                self.fetcher.step(memory);
                if self.fetcher.fifo.len() <= 8 {
                    return false;
                }
                let pixel = self.fetcher.fifo.pop_back();
                display.write(pixel.unwrap());
                self.x += 1;

                if self.x == 160 {
                    self.state = State::HBlank;
                    display.h_blank();
                }
            },
            State::HBlank => {
                if self.ticks >= 456 {
                    self.ticks = 0;
                    //println!("ly = {} state = {:?}", self.read_reg8(Ly, memory),self.state);
                    self.inc_reg(1, memory, Ly);
                    if self.read_reg8(Ly, memory) == 144 {
                        display.v_blank();
                        self.state = State::VBlank;
                        return true;
                    } else {
                        self.state = State::OamSearch;
                    }
                }
            },
            State::VBlank => {
                if self.ticks == 456 {
                    self.ticks = 0;
                    //println!("ly = {} state = {:?}", self.read_reg8(Ly, memory),self.state);
                    self.inc_reg(1, memory, Ly);
                    if self.read_reg8(Ly, memory) == 153 {
                        // End of VBlank, back to initial state.
                        self.write_reg8(Ly, memory, 0);
                        self.state = State::OamSearch;
                    }
                }
            }
        }
        return false;
    }
}

