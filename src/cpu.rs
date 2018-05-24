
pub struct Cpu {
    // Processor registers

    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub pc: u16,
    pub flags: u8,
    pub sp: u8,

    // Other processor state

    pub mem: [u8; 65536],
    pub cycle: usize,

    rd_mask: u64,
    wr_mask: u64,
}

pub trait Bus {
    fn read(&mut self, addr: u16, cpu: &mut Cpu) -> u8;

    fn write(&mut self, addr: u16, val: u8, cpu: &mut Cpu);
}

impl Cpu {
    pub fn new() -> Cpu {
        Cpu {
            a: 0,
            x: 0,
            y: 0,
            pc: 0,
            flags: 0,
            sp: 0,

            mem: [0; 65536],
            cycle: 0,
            rd_mask: 0,
            wr_mask: 0,
        }
    }

    pub fn set_masks(&mut self, rd_mask: u64, wr_mask: u64) {
        self.rd_mask = rd_mask;
        self.wr_mask = wr_mask;
    }

    fn read(&mut self, addr: u16, bus: &mut Bus) -> u8 {
        let result = if self.rd_mask & (1 << (addr >> 10)) != 0 {
            bus.read(addr, self)
        } else {
            self.mem[addr as usize]
        };
        self.cycle += 1;
        result
    }

    fn zp_read(&mut self, addr: u8) -> u8 {
        let result = self.mem[usize::from(addr)];
        self.cycle += 1;
        result
    }

    fn write(&mut self, addr: u16, val: u8, bus: &mut Bus) {
        if self.wr_mask & (1 << (addr >> 10)) != 0 {
            bus.write(addr, val, self)
        } else {
            self.mem[addr as usize] = val;
        }
        self.cycle += 1;
    }

    fn zp_write(&mut self, addr: u8, val: u8, bus: &mut Bus) {
        // TODO: maybe optimize
        self.write(u16::from(addr), val, bus);
    }

    pub fn step(&mut self, bus: &mut Bus) {
        let pc = self.pc;
        let ins = self.read(pc, bus);
        self.pc = self.pc.wrapping_add(1);
        match ins {
            0x01 => { // ORA X,ind
                let a = self.a | self.read_x_ind(bus);
                self.set_a_nz(a);
            }
            0x05 => { // ORA zpg
                let a = self.a | self.read_zp(bus);
                self.set_a_nz(a);
            }
            0x06 => { // ASL zpg
                let (val, addr) = self.rd_wr_zp(bus);
                let new_val = self.asl(val);
                self.zp_write(addr, new_val, bus);
            }
            0x08 => { // PHP
                let flags = self.flags;
                self.push(flags, bus);
            }
            0x09 => { // ORA #
                let a = self.a | self.imm(bus);
                self.set_a_nz(a);
            }
            0x0a => { // ASL A
                let a = self.a;
                let new_val = self.asl(a);
                self.set_a_nz(new_val);
            }
            0x0d => { // ORA abs
                let a = self.a | self.read_abs(bus);
                self.set_a_nz(a);
            }
            0x0e => { // ASL abs
                let (val, addr) = self.rd_wr_abs(bus);
                let new_val = self.asl(val);
                self.write(addr, new_val, bus);
            }

            0x10 => { // BPL rel
                let cond = (self.flags & 0x80) == 0;
                self.cond_branch(cond, bus);
            }
            0x11 => { // ORA ind,Y
                let a = self.a | self.read_ind_y(bus);
                self.set_a_nz(a);
            }
            0x15 => { // ORA zpg,X
                let a = self.a | self.read_zp_x(bus);
                self.set_a_nz(a);
            }
            0x16 => { // ASL zpg,X
                let (val, addr) = self.rd_wr_zp_x(bus);
                let new_val = self.asl(val);
                self.zp_write(addr, new_val, bus);
            }
            0x18 => { // CLC
                self.flags &= !0x01;
                self.waste_cycle(bus);
            }
            0x19 => { // ORA abs,Y
                let a = self.a | self.read_abs_y(bus);
                self.set_a_nz(a);
            }
            0x1d => { // ORA abs,X
                let a = self.a | self.read_abs_x(bus);
                self.set_a_nz(a);
            }
            0x1e => { // ASL abs,X
                let (val, addr) = self.rd_wr_abs_x(bus);
                let new_val = self.asl(val);
                self.write(addr, new_val, bus);
            }

            0x20 => { // JSR abs
                self.jsr(bus);
            }
            0x21 => { // AND X,ind
                let a = self.a & self.read_x_ind(bus);
                self.set_a_nz(a);
            }
            0x24 => { // BIT zpg
                let val = self.read_zp(bus);
                self.bit(val);
            }
            0x25 => { // AND zpg
                let a = self.a & self.read_zp(bus);
                self.set_a_nz(a);
            }
            0x26 => { // ROL zpg
                let (val, addr) = self.rd_wr_zp(bus);
                let new_val = self.rol(val);
                self.zp_write(addr, new_val, bus);
            }
            0x28 => { // PLP
                self.flags = self.pull(bus);
            }
            0x29 => { // AND #
                let a = self.a & self.imm(bus);
                self.set_a_nz(a);
            }
            0x2a => { // ROL A
                let a = self.a;
                let new_val = self.rol(a);
                self.set_a_nz(new_val);
            }
            0x2c => { // BIT abs
                let val = self.read_abs(bus);
                self.bit(val);
            }
            0x2d => { // AND abs
                let a = self.a & self.read_abs(bus);
                self.set_a_nz(a);
            }
            0x2e => { // ROL abs
                let (val, addr) = self.rd_wr_abs(bus);
                let new_val = self.rol(val);
                self.write(addr, new_val, bus);
            }

            0x30 => { // BMI rel
                let cond = (self.flags & 0x80) != 0;
                self.cond_branch(cond, bus);
            }
            0x31 => { // AND ind,Y
                let a = self.a & self.read_ind_y(bus);
                self.set_a_nz(a);
            }
            0x35 => { // AND zpg,X
                let a = self.a & self.read_zp_x(bus);
                self.set_a_nz(a);
            }
            0x36 => { // ROL zpg,X
                let (val, addr) = self.rd_wr_zp_x(bus);
                let new_val = self.rol(val);
                self.zp_write(addr, new_val, bus);
            }
            0x38 => { // SEC
                self.flags |= 0x01;
                self.waste_cycle(bus);
            }
            0x39 => { // AND abs,Y
                let a = self.a & self.read_abs_y(bus);
                self.set_a_nz(a);
            }
            0x3d => { // AND abs,X
                let a = self.a & self.read_abs_x(bus);
                self.set_a_nz(a);
            }
            0x3e => { // ROL abs,X
                let (val, addr) = self.rd_wr_abs_x(bus);
                let new_val = self.rol(val);
                self.write(addr, new_val, bus);
            }

            0x8d => { // STA abs
                let a = self.a;
                self.write_abs(a, bus);
            }
            0xa9 => { // LDA #
                let a = self.imm(bus);
                self.set_a_nz(a);
            }
            _ => println!("unimpl ins {:02x}", ins),
        }
    }

    fn push(&mut self, val: u8, bus: &mut Bus) {
        self.waste_cycle(bus);
        let addr = 0x100 | u16::from(self.sp);
        self.write(addr, val, bus);
        self.sp = self.sp.wrapping_sub(1);
    }

    fn pull(&mut self, bus: &mut Bus) -> u8 {
        self.waste_cycle(bus);
        let addr = 0x100 | u16::from(self.sp);
        let _ = self.read(addr, bus);  // can optimize, is never peripheral
        self.sp = self.sp.wrapping_add(1);
        let addr = 0x100 | u16::from(self.sp);
        self.read(addr, bus)
    }

    fn cond_branch(&mut self, cond: bool, bus: &mut Bus) {
        let rel = self.imm(bus);
        if cond {
            let pc = self.pc;
            let _ = self.read(pc, bus);
            let new_pc = pc.wrapping_add(i16::from(rel as i8) as u16);
            let nocarry_pc = (pc & 0xff00) | (new_pc & 0xff);
            if new_pc != nocarry_pc {
                let _ = self.read(nocarry_pc, bus);
            }
            self.pc = new_pc;
        }
    }

    // More complex instructions

    fn jsr(&mut self, bus: &mut Bus) {
        let target_lo = self.imm(bus);
        let addr = 0x100 | u16::from(self.sp);
        let _ = self.read(addr, bus);
        let hi = (self.pc >> 8) as u8;
        self.write(addr, hi, bus);
        self.sp = self.sp.wrapping_sub(1);
        let addr = 0x100 | u16::from(self.sp);
        let lo = self.pc as u8;
        self.write(addr, lo, bus);
        self.sp = self.sp.wrapping_sub(1);
        let target_hi = self.imm(bus);
        self.pc = (u16::from(target_hi) << 8) | u16::from(target_lo);
    }

    // Addressing modes

    fn waste_cycle(&mut self, bus: &mut Bus) {
        let pc = self.pc;
        let _ = self.read(pc, bus);
    }

    fn imm(&mut self, bus: &mut Bus) -> u8 {
        let pc = self.pc;
        let result = self.read(pc, bus);
        self.pc = self.pc.wrapping_add(1);
        result
    }

    fn abs_addr(&mut self, bus: &mut Bus) -> u16 {
        let pc = self.pc;
        let lo = self.read(pc, bus);
        let hi = self.read(pc.wrapping_add(1), bus);
        self.pc = self.pc.wrapping_add(2);
        (u16::from(hi) << 8) | u16::from(lo)
    }

    fn abs_ix_addr(&mut self, ix: u8, bus: &mut Bus) -> u16 {
        let pc = self.pc;
        let lo = self.read(pc, bus);
        let mut hi = self.read(pc.wrapping_add(1), bus);
        let (new_lo, carry) = lo.overflowing_add(ix);
        if carry {
            let _ = self.read((u16::from(hi) << 8) | u16::from(lo), bus);
            hi += 1;
        }
        self.pc = self.pc.wrapping_add(2);
        (u16::from(hi) << 8) | u16::from(new_lo)
    }

    fn abs_x_addr(&mut self, bus: &mut Bus) -> u16 {
        let x = self.x;
        self.abs_ix_addr(x, bus)
    }

    fn abs_y_addr(&mut self, bus: &mut Bus) -> u16 {
        let y = self.y;
        self.abs_ix_addr(y, bus)
    }

    fn read_x_ind(&mut self, bus: &mut Bus) -> u8 {
        let addr = self.zp_x_addr(bus);
        let lo = self.zp_read(addr);
        let hi = self.zp_read(addr.wrapping_add(1));
        self.read((u16::from(hi) << 8) | u16::from(lo), bus)
    }

    fn read_ind_y(&mut self, bus: &mut Bus) -> u8 {
        let addr = self.imm(bus);
        let lo = self.zp_read(addr);
        let mut hi = self.zp_read(addr.wrapping_add(1));
        let (new_lo, carry) = lo.overflowing_add(self.y);
        if carry {
            let _ = self.read((u16::from(hi) << 8) | u16::from(lo), bus);
            hi += 1;
        }
        self.read((u16::from(hi) << 8) | u16::from(new_lo), bus)
    }

    fn zp_addr(&mut self, bus: &mut Bus) -> u8 {
        self.imm(bus)
    }

    fn read_zp(&mut self, bus: &mut Bus) -> u8 {
        let addr = self.zp_addr(bus);
        self.zp_read(addr)
    }

    fn rd_wr_zp(&mut self, bus: &mut Bus) -> (u8, u8) {
        let addr = self.zp_addr(bus);
        let val = self.zp_read(addr);
        self.zp_write(addr, val, bus);  // wasted cycle
        (val, addr)
    }

    fn zp_x_addr(&mut self, bus: &mut Bus) -> u8 {
        let addr = self.zp_addr(bus);
        let _ = self.zp_read(addr);
        addr.wrapping_add(self.x)
    }

    fn read_zp_x(&mut self, bus: &mut Bus) -> u8 {
        let addr = self.zp_x_addr(bus);
        self.zp_read(addr)
    }

    fn rd_wr_zp_x(&mut self, bus: &mut Bus) -> (u8, u8) {
        let addr = self.zp_x_addr(bus);
        let val = self.zp_read(addr);
        self.zp_write(addr, val, bus);  // wasted cycle
        (val, addr)
    }

    fn read_abs(&mut self, bus: &mut Bus) -> u8 {
        let addr = self.abs_addr(bus);
        self.read(addr, bus)
    }

    fn write_abs(&mut self, val: u8, bus: &mut Bus) {
        let addr = self.abs_addr(bus);
        self.write(addr, val, bus);
    }

    fn rd_wr_abs(&mut self, bus: &mut Bus) -> (u8, u16) {
        let addr = self.abs_addr(bus);
        let val = self.read(addr, bus);
        self.write(addr, val, bus);  // wasted cycle
        (val, addr)
    }

    fn read_abs_x(&mut self, bus: &mut Bus) -> u8 {
        let addr = self.abs_x_addr(bus);
        self.read(addr, bus)
    }

    fn rd_wr_abs_x(&mut self, bus: &mut Bus) -> (u8, u16) {
        let addr = self.abs_addr(bus);
        let new_addr = addr.wrapping_add(u16::from(self.x));
        let addr_no_carry = (addr & 0xff00) | (new_addr & 0xff);
        let _ = self.read(addr_no_carry, bus);
        let val = self.read(new_addr, bus);
        self.write(addr, val, bus);  // wasted cycle
        (val, addr)
    }

    fn read_abs_y(&mut self, bus: &mut Bus) -> u8 {
        let addr = self.abs_y_addr(bus);
        self.read(addr, bus)
    }


    fn set_a_nz(&mut self, a: u8) {
        self.a = a;
        self.flags = (self.flags & 0x7d) | (a & 0x80) | if a == 0 { 2 } else { 0 };
    }

    // Calculations

    fn asl(&mut self, val: u8) -> u8 {
        let c = (val & 0x80) >> 7;
        let new_val = val.wrapping_shl(1);
        self.flags = (self.flags & 0x7c) | (new_val & 0x80) | c
            | if new_val == 0 { 2 } else { 0 };
        new_val
    }

    fn bit(&mut self, val: u8) {
        self.flags = (self.flags & 0x3d) | (val & 0xc0)
            | if val & self.a == 0 { 2 } else { 0 };
    }

    fn rol(&mut self, val: u8) -> u8 {
        let c = (val & 0x80) >> 7;
        let new_val = val.wrapping_shl(1) | (self.flags & 1);
        self.flags = (self.flags & 0x7c) | (new_val & 0x80) | c
            | if new_val == 0 { 2 } else { 0 };
        new_val
    }

}
