
pub struct Cpu {
    // Processor registers

    a: u8,
    x: u8,
    y: u8,
    pc: u16,
    flags: u8,
    sp: u8,

    // Other processor state

    mem: [u8; 65536],
    cycle: usize,

    rd_mask: u64,
    wr_mask: u64,
}

pub trait Bus {
    fn read(&mut self, addr: u16, cycle: usize) -> u8;

    fn write(&mut self, addr: u16, val: u8, cycle: usize);
}

impl Cpu {
    fn new() -> Cpu {
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

    fn read(&mut self, addr: u16, bus: &mut Bus) -> u8 {
        let result = if self.rd_mask & (1 << (addr >> 10)) != 0 {
            let cycle =  self.cycle;
            bus.read(addr, cycle)
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
            let cycle = self.cycle;
            bus.write(addr, val, cycle)
        } else {
            self.mem[addr as usize] = val;
        }
        self.cycle += 1;
    }

    fn zp_write(&mut self, addr: u8, val: u8, bus: &mut Bus) {
        // TODO: maybe optimize
        self.write(u16::from(addr), val, bus);
    }

    fn step(&mut self, bus: &mut Bus) {
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

    fn abs(&mut self, bus: &mut Bus) -> u16 {
        let pc = self.pc;
        let lo = self.read(pc, bus);
        let hi = self.read(pc.wrapping_add(1), bus);
        self.pc = self.pc.wrapping_add(2);
        (u16::from(hi) << 8) | u16::from(lo)
    }

    fn abs_ix(&mut self, ix: u8, bus: &mut Bus) -> u16 {
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

    fn abs_x(&mut self, bus: &mut Bus) -> u16 {
        let x = self.x;
        self.abs_ix(x, bus)
    }

    fn abs_y(&mut self, bus: &mut Bus) -> u16 {
        let y = self.y;
        self.abs_ix(y, bus)
    }

    fn read_x_ind(&mut self, bus: &mut Bus) -> u8 {
        let pc = self.pc;
        let x = self.x;
        let addr = self.read(pc, bus);
        let _ = self.zp_read(addr);  // wasted cycle
        let addr = addr.wrapping_add(x);
        let lo = self.zp_read(addr);
        let hi = self.zp_read(addr.wrapping_add(1));
        let result = self.read((u16::from(hi) << 8) | u16::from(lo), bus);
        self.pc = self.pc.wrapping_add(1);
        result
    }

    fn zp(&mut self, bus: &mut Bus) -> u8 {
        let pc = self.pc;
        let result = self.read(pc, bus);
        self.pc = self.pc.wrapping_add(1);
        result
    }

    fn read_zp(&mut self, bus: &mut Bus) -> u8 {
        let addr = self.zp(bus);
        self.zp_read(addr)
    }

    fn read_abs(&mut self, bus: &mut Bus) -> u8 {
        let addr = self.abs(bus);
        self.read(addr, bus)
    }

    fn rd_wr_zp(&mut self, bus: &mut Bus) -> (u8, u8) {
        let addr = self.zp(bus);
        let val = self.zp_read(addr);
        self.zp_write(addr, val, bus);  // wasted cycle
        (val, addr)
    }

    fn write_abs(&mut self, val: u8, bus: &mut Bus) {
        let addr = self.abs(bus);
        self.write(addr, val, bus);
    }

    fn set_a_nz(&mut self, a: u8) {
        self.a = a;
        self.flags = (self.flags & 0x3f) | (a & 0x80) | if a == 0 { 0x40 } else { 0 };
    }

    // calculations
    fn asl(&mut self, val: u8) -> u8 {
        let c = (val & 0x80) >> 2;
        let new_val = val.wrapping_shl(1);
        self.flags = (self.flags & 0x1f) | (new_val & 0x80) | c
            | if new_val == 0 { 0x40 } else { 0 };
        new_val
    }
}
