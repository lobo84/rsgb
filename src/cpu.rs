use std::fmt;
use std::process::exit;
use Flag::*;
use Register16::*;
use Register8::*;


use crate::mmu::Memory;

macro_rules! print_function_name {
        ($a:expr) => {{
            fn f() {}
            fn type_name_of<T>(_: T) -> &'static str {
                std::any::type_name::<T>()
            }
            let name = type_name_of(f);

            // Find and cut the rest of the path
            match &name[..name.len() - 3].rfind(':') {
                Some(pos) => {
                    let n = &name[pos + 1..name.len() - 3];
                    //println!("{:?} : {}", $a, n);
                    n
                }
                None => &name[..name.len() - 3],
            }
        }};
    }


fn get_bit_at(input: u8, n: u8) -> bool {
    return input & (1 << n) != 0;
}

fn set_bit_at(input: u8, n: u8, val: bool) -> u8 {
    return if val {
        input | 1 << n
    } else {
        input & !(1 << n)
    };
}

impl Register16 {
    fn byte_index(self) -> usize {
        return match self {
            PC => 8,
            SP => 10,
            HL => Register8::L.byte_index(),
            BC => Register8::C.byte_index(),
            DE => Register8::E.byte_index(),
            AF => Register8::F.byte_index(),
        } as usize;
    }
}

impl Register8 {
    fn byte_index(self) -> usize {
        return match self {
            Register8::A => 1,
            Register8::F => 0,
            Register8::B => 3,
            Register8::C => 2,
            Register8::D => 4,
            Register8::E => 5,
            Register8::H => 7,
            Register8::L => 6,
        } as usize;
    }
}

#[derive(Debug, Copy, Clone)]
enum Flag { Carry = 4, HalfCarry, Negative, Zero }


#[derive(Debug, Copy, Clone)]
enum Register8 {
    A,
    F,
    B,
    C,
    D,
    E,
    H,
    L,
}

#[derive(Debug, Copy, Clone)]
enum Register16 {
    PC,
    SP,
    HL,
    BC,
    DE,
    AF,
}

pub struct Cpu {
    cycle: u8,
    registers: [u8; 12],
}

impl fmt::Debug for Cpu {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,
               "A: 0x{:x?} B: 0x{:x?} C: 0x{:x?} D: 0x{:x?} E: 0x{:x?} H: 0x{:x?} L: 0x{:x?} PC: 0x{:x?} SP: 0x{:x?}",
               self.read_reg8(A),
               self.read_reg8(B),
               self.read_reg8(C),
               self.read_reg8(D),
               self.read_reg8(E),
               self.read_reg8(H),
               self.read_reg8(L),
               self.read_reg16(PC),
               self.read_reg16(SP),
        )
    }
}

impl Default for Cpu {
    fn default() -> Self {
        Self {
            cycle: 0,
            registers: [0; 12],
        }
    }
}

impl Cpu {
    fn op_ld_n_nn(&mut self, reg: Register16, memory: &Memory) -> u8 {
        let pc = self.read_reg16(PC);
        self.write_reg16(reg, memory.read16(pc + 1));
        self.inc_pc(3);
        print_function_name!(self);
        return 12;
    }

    fn op_ld_n_n(&mut self, reg: Register8, memory: &Memory) -> u8 {
        let pc = self.read_reg16(PC);
        self.write_reg8(memory.read8(pc + 1), reg);
        self.inc_pc(2);
        print_function_name!(self);
        return 2;
    }

    fn op_ld_r_mr(&mut self, dst_reg: Register8, addr_reg: Register16, memory: &Memory) -> u8 {
        let addr = self.read_reg16(addr_reg);
        let val = memory.read8(addr);
        self.write_reg8(val, dst_reg);
        self.inc_pc(1);
        print_function_name!(self);
        return 2;
    }

    fn op_ld_hl_a(&mut self, memory: &mut Memory, inc: i32) -> u8 {
        let a = self.read_reg8(A);
        let hl = self.read_reg16(HL);
        memory.write8(hl, a);
        self.write_reg16(HL, (hl as i32 + inc) as u16);
        self.inc_pc(1);
        print_function_name!(self);
        return 8;
    }

    fn op_xor(&mut self, reg: Register8) -> u8 {
        let r = self.read_reg8(reg);
        let a = self.read_reg8(A);
        let new_a = a ^ r;
        self.clear_flags();
        if new_a == 0 {
            self.set_flag(Zero, true);
        }
        self.write_reg8(new_a, A);
        self.inc_pc(1);
        print_function_name!(self);
        return 4;
    }

    fn op_bc_bit(&mut self, bit: u8, reg: Register8) -> u8 {
        let val = Cpu::read_bit(bit, self.read_reg8(reg));
        self.set_flag(Zero, !val);
        self.inc_pc(2);
        print_function_name!(self);
        return 8;
    }

    fn inc_pc(&mut self, inc: i32) {
        self.inc16(PC, inc);
    }

    fn inc16(&mut self, reg: Register16, inc: i32) {
        let val = self.read_reg16(reg);
        self.write_reg16(reg, (val as i32 + inc) as u16);
    }

    fn inc8(&mut self, reg: Register8, inc: i32) {
        let val = self.read_reg8(reg);
        self.write_reg8((val as i32 + inc) as u8, reg);
    }

    fn one_bit(bit: u8, value: u8) -> u8 {
        return value | 1 << bit;
    }

    fn zero_bit(bit: u8, value: u8) -> u8 {
        return value & !(1 << bit);
    }

    fn set_bit(bit: u8, value: u8, bit_value: bool) -> u8 {
        return match bit_value {
            true => Cpu::one_bit(bit, value),
            false => Cpu::zero_bit(bit, value),
        };
    }

    fn read_bit(bit: u8, value: u8) -> bool {
        return ((value & (1 << bit)) >> bit) != 0;
    }

    fn set_flag(&mut self, flag: Flag, value: bool) {
        let f_val = self.read_reg8(F);
        self.write_reg8(Cpu::set_bit(Cpu::bit(flag), f_val, value), F);
    }

    fn get_flag(&self, flag: Flag) -> bool {
        let val = self.read_reg8(F);
        return Cpu::read_bit(Cpu::bit(flag), val);
    }

    fn bit(flag: Flag) -> u8 {
        return match flag {
            Zero => 7,
            Negative => 6,
            HalfCarry => 5,
            Carry => 4,
        };
    }

    fn clear_flags(&mut self) {
        self.set_flag(Zero, false);
        self.set_flag(Negative, false);
        self.set_flag(HalfCarry, false);
        self.set_flag(Carry, false);
    }

    fn read_reg16(&self, reg: Register16) -> u16 {
        let lo = self.registers[reg.byte_index()] as u16;
        let hi = self.registers[reg.byte_index() + 1] as u16;
        return (hi << 8) | lo;
    }

    fn read_reg8(&self, reg: Register8) -> u8 {
        return self.registers[reg.byte_index()];
    }

    fn write_reg8(&mut self, val: u8, reg: Register8) {
        self.registers[reg.byte_index()] = val;
    }

    fn write_reg16(&mut self, reg: Register16, value: u16) {
        let lo = (value & 0x00ff) as u8;
        let hi = (value >> 8) as u8;
        self.registers[reg.byte_index()] = lo;
        self.registers[reg.byte_index() + 1] = hi;
    }

    fn op_cb(&mut self, memory: &mut Memory) -> u8 {
        let pc = self.read_reg16(PC) + 1;
        let opcode = memory.read8(pc);
        //println!("0x{:x?}: 0xcb{:x?}", pc, opcode);
        return match opcode {
            0x78 => self.op_bc_bit(7, B),
            0x79 => self.op_bc_bit(7, C),
            0x7a => self.op_bc_bit(7, D),
            0x7b => self.op_bc_bit(7, E),
            0x7c => self.op_bc_bit(7, H),
            0x7d => self.op_bc_bit(7, L),
            0x17 => {
                self.op_rl(A, 2);
                8
            }
            0x10 => {
                self.op_rl(B, 2);
                8
            }
            0x11 => {
                self.op_rl(C, 2);
                8
            }
            0x12 => {
                self.op_rl(D, 2);
                8
            }
            0x13 => {
                self.op_rl(E, 2);
                8
            }
            0x14 => {
                self.op_rl(H, 2);
                8
            }
            0x15 => {
                self.op_rl(L, 2);
                8
            }
            _ => {
                println!("opcode not implemented 0xCB{:x?}", opcode);
                exit(1)
            }
        };
    }

    pub fn step(&mut self, clock: u32, memory: &mut Memory) {
        let pc = self.read_reg16(PC);
        let opcode = memory.read8(pc);

        if self.cycle != 0 {
            self.cycle -= 1;
            return;
        }
        println!("0x{:x?}: 0x{:x?} : {:?}", pc, opcode,self);
        self.cycle = match opcode {
            0xaf => self.op_xor(A),
            0x17 => {
                self.op_rl(A, 1);
                4
            }
            0x01 => self.op_ld_n_nn(BC, memory),
            0x11 => self.op_ld_n_nn(DE, memory),
            0x21 => self.op_ld_n_nn(HL, memory),
            0x31 => self.op_ld_n_nn(SP, memory),
            0x32 => self.op_ld_hl_a(memory, -1),
            0x22 => self.op_ld_hl_a(memory, 1),
            0x77 => self.op_ld_hl_a(memory, 0),

            0x47 => self.op_ld_r_r(A, B),
            0x4f => self.op_ld_r_r(A, C),
            0x57 => self.op_ld_r_r(A, D),
            0x5f => self.op_ld_r_r(A, E),
            0x67 => self.op_ld_r_r(A, H),
            0x6f => self.op_ld_r_r(A, L),

            0x7f => self.op_ld_r_r(A, A),
            0x78 => self.op_ld_r_r(B, A),
            0x79 => self.op_ld_r_r(C, A),
            0x7a => self.op_ld_r_r(D, A),
            0x7b => self.op_ld_r_r(E, A),
            0x7c => self.op_ld_r_r(H, A),
            0x7d => self.op_ld_r_r(L, A),

            0xbf => {
                self.op_cp(self.read_reg8(A), 1);
                4
            }
            0xb8 => {
                self.op_cp(self.read_reg8(B), 1);
                4
            }
            0xb9 => {
                self.op_cp(self.read_reg8(C), 1);
                4
            }
            0xba => {
                self.op_cp(self.read_reg8(D), 1);
                4
            }
            0xbb => {
                self.op_cp(self.read_reg8(E), 1);
                4
            }
            0xbc => {
                self.op_cp(self.read_reg8(H), 1);
                4
            }
            0xbd => {
                self.op_cp(self.read_reg8(L), 1);
                4
            }
            0xfe => {
                self.op_cp(memory.read8(self.read_reg16(PC) + 1), 2);
                8
            }

            0xcb => self.op_cb(memory),
            0x20 => self.op_jr(memory, Option::Some(|c| !c.get_flag(Zero))),
            0x28 => self.op_jr(memory, Option::Some(|c| c.get_flag(Zero))),
            0x30 => self.op_jr(memory, Option::Some(|c| !c.get_flag(Carry))),
            0x38 => self.op_jr(memory, Option::Some(|c| c.get_flag(Carry))),
            0x18 => self.op_jr(memory, Option::None),
            0x80 => self.op_add(B),
            0x05 => self.op_dec8(B),
            0x15 => self.op_dec8(D),
            0x25 => self.op_dec8(H),
            0x3d => self.op_dec8(A),
            0x0d => self.op_dec8(C),
            0x1d => self.op_dec8(E),
            0xe2 => self.op_ld_c_a(memory),
            0x3C => self.op_inc8(A),
            0x04 => self.op_inc8(B),
            0x0c => self.op_inc8(C),
            0x14 => self.op_inc8(D),
            0x1C => self.op_inc8(E),
            0x24 => self.op_inc8(H),
            0x2c => self.op_inc8(L),
            0x97 => self.sub_r_r(A, A, A),
            0x90 => self.sub_r_r(A, A, B),
            0x91 => self.sub_r_r(A, A, C),
            0x92 => self.sub_r_r(A, A, D),
            0x93 => self.sub_r_r(A, A, E),
            0x94 => self.sub_r_r(A, A, H),
            0x95 => self.sub_r_r(A, A, L),
            0x34 => {
                self.op_inc16(HL);
                12
            }
            0x03 => {
                self.op_inc16(BC);
                8
            }
            0x13 => {
                self.op_inc16(DE);
                8
            }
            0x23 => {
                self.op_inc16(HL);
                8
            }
            0x33 => {
                self.op_inc16(SP);
                8
            }
            0x3e => self.op_ld_n_n(A, memory),
            0x06 => self.op_ld_n_n(B, memory),
            0x0E => self.op_ld_n_n(C, memory),
            0x16 => self.op_ld_n_n(D, memory),
            0x1e => self.op_ld_n_n(E, memory),
            0x26 => self.op_ld_n_n(H, memory),
            0x2e => self.op_ld_n_n(L, memory),
            0xe0 => self.op_ld_a8_r(A, memory, 0xFF00),
            0xf0 => self.op_ld_r_a8(A, memory, 0xFF00),
            0xea => self.op_ld_a16_r(A, memory),
            0x1a => self.op_ld_r_mr(A, DE, memory),
            0xcd => self.op_call(memory),
            0xc9 => self.op_ret(memory),
            0xc5 => {
                self.op_push16(BC, memory);
                16
            }
            0xd5 => {
                self.op_push16(DE, memory);
                16
            }
            0xe5 => {
                self.op_push16(HL, memory);
                16
            }
            0xf5 => {
                self.op_push16(AF, memory);
                16
            }
            0xc1 => self.op_pop16(BC, memory),
            0x00 => self.op_nop(),
            _ => {
                println!("opcode not implemented 0x{:x?}", opcode);
                exit(1)
            }
        };
        assert!(self.cycle > 0);
        //Cpu{registers: Registers{sp: sp, ..self.registers}, ..*self }
    }

    fn op_jr(
        &mut self,
        memory: &mut Memory,
        jump_condition: Option<fn(cpu: &Cpu) -> bool>,
    ) -> u8 {
        let pc = self.read_reg16(PC);
        self.inc_pc(2);
        print_function_name!(self);
        if jump_condition.is_some() {
            return if jump_condition.unwrap()(self) {
                let n = memory.read8(pc + 1);
                self.inc_pc((n as i8) as i32);
                12
            } else {
                8
            };
        } else {
            let n = memory.read8(pc + 1);
            self.inc_pc((n as i8) as i32);
            8
        }
    }

    fn op_add(&mut self, reg: Register8) -> u8 {
        let b = self.read_reg8(reg);
        let a = self.read_reg8(A);
        let (val, carry) = a.overflowing_add(b);
        let half_carry = (a & 0xF) + (b & 0xF) > 0xF;
        self.set_flag(Carry, carry);
        self.set_flag(HalfCarry, half_carry);
        self.set_flag(Zero, val == 0);
        self.set_flag(Negative, false);
        self.write_reg8(val, A);
        self.inc_pc(1);
        print_function_name!(self);
        return 1;
    }

    fn op_ld_c_a(&mut self, memory: &mut Memory) -> u8 {
        let a = self.read_reg8(A);
        let c = self.read_reg8(C) as u16;
        memory.write8(0xFF00 + c, a);
        self.inc_pc(1);
        print_function_name!(self);
        return 2;
    }

    fn op_ld_a8_r(&mut self, reg: Register8, memory: &mut Memory, offset: u16) -> u8 {
        let a = self.read_reg8(reg);
        let val = memory.read8(self.read_reg16(PC) + 1) as u16;
        memory.write8(offset + val, a);
        self.inc_pc(2);
        print_function_name!(self);
        return 12;
    }

    fn op_ld_r_a8(&mut self, reg: Register8, memory: &mut Memory, offset: u16) -> u8 {
        let val = memory.read8(self.read_reg16(PC) + 1) as u16;
        self.write_reg8(memory.read8(offset + val), reg);
        self.inc_pc(2);
        print_function_name!(self);
        return 12;
    }

    fn op_ld_a16_r(&mut self, reg: Register8, memory: &mut Memory) -> u8 {
        let a = self.read_reg8(reg);
        let val = memory.read16(self.read_reg16(PC) + 1);
        memory.write8(val, a);
        self.inc_pc(3);
        print_function_name!(self);
        return 3;
    }

    fn reg_math_2(
        &mut self,
        dest: Register8,
        op1: Register8,
        op2: Register8,
        operation: fn(u8, u8) -> (u8, bool),
        subtraction: bool,
    ) -> u8 {
        let val1 = self.read_reg8(op1);
        let val2 = self.read_reg8(op2);
        let half_carry = operation(val1 & 0xF, val2 & 0xF).0 > 0xF;
        let (r, carry) = operation(val1, val2);
        self.set_flag(Carry, carry);
        self.set_flag(HalfCarry, half_carry);
        self.set_flag(Zero, r == 0);
        self.set_flag(Negative, subtraction);
        self.write_reg8(r, dest);
        self.inc_pc(1);
        print_function_name!(self);
        return 4;
    }

    fn op_reg8_math_1(
        &mut self,
        dest: Register8,
        op1: Register8,
        operation: fn(u8) -> (u8, bool),
        subtraction: bool,
    ) {
        let val1 = self.read_reg8(op1);
        let half_carry = operation(val1).0 > 0xF;
        let (r, carry) = operation(val1);
        self.set_flag(Carry, carry);
        self.set_flag(HalfCarry, half_carry);
        self.set_flag(Zero, r == 0);
        self.set_flag(Negative, subtraction);
        self.write_reg8(r, dest);
        self.inc_pc(1);
        print_function_name!(self);
    }

    fn op_reg16_math_1(
        &mut self,
        dest: Register16,
        op1: Register16,
        operation: fn(u16) -> (u16, bool),
        subtraction: bool,
    ) {
        let val1 = self.read_reg16(op1);
        let half_carry = operation(val1).0 > 0xF;
        let (r, carry) = operation(val1);
        self.set_flag(Carry, carry);
        self.set_flag(HalfCarry, half_carry);
        self.set_flag(Zero, r == 0);
        self.set_flag(Negative, subtraction);
        self.write_reg16(dest, r);
        self.inc_pc(1);
        print_function_name!(self);
    }

    fn sub_r_r(&mut self, dst: Register8, reg1: Register8, reg2: Register8) -> u8 {
        return self.reg_math_2(dst, reg1, reg2, |op1, op2| op1.overflowing_sub(op2), true);
    }

    fn op_call(&mut self, memory: &mut Memory) -> u8 {
        let pc = self.read_reg16(PC);
        let jmp_addr = memory.read16(pc + 1);
        let return_addr = pc + 3;
        self.push16(memory, return_addr);
        self.write_reg16(PC, jmp_addr);
        print_function_name!(self);
        return 12;
    }

    fn op_nop(&mut self) -> u8 {
        self.inc_pc(1);
        print_function_name!(self);
        return 1;
    }

    fn op_ld_r_r(&mut self, src: Register8, dst: Register8) -> u8 {
        self.write_reg8(self.read_reg8(src), dst);
        self.inc_pc(1);
        print_function_name!(self);
        return 1;
    }

    fn op_cp(&mut self, n: u8, size: i32) {
        let a = self.read_reg8(A);
        self.set_flag(Zero, a == n);
        self.set_flag(Negative, true);
        self.set_flag(HalfCarry, a.saturating_sub(n) > 0xF);
        self.set_flag(Carry, a < n);
        self.inc_pc(size);
        print_function_name!(self);
        //println!("{:x?} : {:x?}", n, a);
    }

    fn op_push16(&mut self, reg: Register16, memory: &mut Memory) -> u8 {
        let value = self.read_reg16(reg);
        self.push16(memory, value);
        self.inc_pc(1);
        print_function_name!(self);
        return 16;
    }

    fn push16(&mut self, memory: &mut Memory, value: u16) {
        let sp = self.read_reg16(SP);
        let [hi, lo] = value.to_be_bytes();
        memory.write8(sp - 1, hi);
        memory.write8(sp - 2, lo);
        self.inc16(SP, -3);
    }

    fn pop16(&mut self, memory: &mut Memory, reg: Register16) {
        let sp = self.read_reg16(SP);
        let lo = memory.read8(sp + 1) as u16;
        let hi = memory.read8(sp + 2) as u16;
        self.inc16(SP, 3);
        let val = (hi << 8) | lo;
        self.write_reg16(reg, val)
    }

    fn op_inc8(&mut self, reg: Register8) -> u8 {
        self.op_reg8_math_1(reg, reg, |x| x.overflowing_add(1), false);
        print_function_name!(self);
        return 4;
    }

    fn op_dec8(&mut self, reg: Register8) -> u8 {
        self.op_reg8_math_1(reg, reg, |x| x.overflowing_sub(1), true);
        print_function_name!(self);
        return 4;
    }

    fn op_inc16(&mut self, reg: Register16) {
        self.op_reg16_math_1(reg, reg, |x| x.overflowing_add(1), false);
        print_function_name!(self);
        if matches!(reg, DE) {
            println!("0x{:x?} : cpu = {:?},", self.read_reg16(DE), self);
        }
    }

    fn op_rl(&mut self, reg: Register8, size: u8) {
        let a = self.read_reg8(reg);
        let bit7 = get_bit_at(a, 7);
        let c = self.get_flag(Carry);
        let new_a = set_bit_at(a << 1, 0, c);
        self.write_reg8(new_a, reg);
        self.set_flag(Carry, bit7);
        self.set_flag(Zero, new_a == 0);
        self.set_flag(Negative, false);
        self.set_flag(HalfCarry, false);
        self.inc_pc(size as i32);
        print_function_name!(self);
    }

    fn op_ret(&mut self, memory: &mut Memory) -> u8 {
        self.pop16(memory, PC);
        print_function_name!(self);
        return 8;
    }

    fn op_pop16(&mut self, reg: Register16, mem: &mut Memory) -> u8 {
        self.pop16(mem, reg);
        self.inc_pc(1);
        print_function_name!(self);
        return 12;
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_push_pop() {
        let mut cpu = Cpu::default();
        let mut mem: Memory = Memory::default();
        cpu.write_reg16(SP, 0xFF);
        cpu.push16(&mut mem, 0xab);
        cpu.pop16(&mut mem, BC);
        let bc = cpu.read_reg16(BC);
        assert_eq!(0xab, bc);
    }

    #[test]
    fn test_op_ld_r_r() {
        let mut cpu = Cpu::default();
        cpu.write_reg8(0xab, E);
        cpu.op_ld_r_r(E, A);
        assert_eq!(0xab, cpu.read_reg8(A));
    }

    #[test]
    fn test_op_inc16() {
        let mut cpu = Cpu::default();
        cpu.write_reg8(0xbe, D);
        cpu.write_reg8(0xab, E);
        assert_eq!(0xabbe, cpu.read_reg16(DE));

        cpu.inc16(DE, 1);
        assert_eq!(0xabbe + 1, cpu.read_reg16(DE));
        assert_eq!(0xab, cpu.read_reg8(E));
    }
}
