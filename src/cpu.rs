pub struct Cpu {
    // Processor registers
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub pc: u16,
    pub flags: u8,
    pub sp: u8,

    pub addr: u16,
    pub is_write: bool,
    pub data: u8,
    pub insn: u8,
    pub insn_cycle: u8,
    pub lo: u8,
    pub ptr: u8,

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
            flags: 0x30,
            sp: 0,
            addr: 0,
            is_write: false,
            data: 0,
            insn: 0xea,
            insn_cycle: 0,
            lo: 0,
            ptr: 0,

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

    pub fn print_state(&self) {
        println!(
            "a {:02x} x {:02x} y {:02x} s {:02x} p {:02x} pc {:04x}",
            self.a, self.x, self.y, self.sp, self.flags, self.pc
        );
    }

    fn set_nz(&mut self, a: u8) {
        self.flags = (self.flags & 0x7d) | (a & 0x80) | if a == 0 { 2 } else { 0 };
    }

    fn set_a_nz(&mut self, val: u8) {
        self.a = val;
        self.set_nz(val);
    }

    fn set_x_nz(&mut self, val: u8) {
        self.x = val;
        self.set_nz(val);
    }

    fn set_y_nz(&mut self, val: u8) {
        self.y = val;
        self.set_nz(val);
    }

    // Calculations

    fn asl(&mut self, val: u8) -> u8 {
        let c = (val & 0x80) >> 7;
        let new_val = val.wrapping_shl(1);
        self.flags = (self.flags & 0x7c) | (new_val & 0x80) | c | if new_val == 0 { 2 } else { 0 };
        new_val
    }

    fn bit(&mut self, val: u8) {
        self.flags = (self.flags & 0x3d) | (val & 0xc0) | if val & self.a == 0 { 2 } else { 0 };
    }

    fn rol(&mut self, val: u8) -> u8 {
        let c = (val & 0x80) >> 7;
        let new_val = val.wrapping_shl(1) | (self.flags & 1);
        self.flags = (self.flags & 0x7c) | (new_val & 0x80) | c | if new_val == 0 { 2 } else { 0 };
        new_val
    }

    fn lsr(&mut self, val: u8) -> u8 {
        let c = val & 1;
        let new_val = val >> 1;
        self.flags = (self.flags & 0x7c) | (new_val & 0x80) | c | if new_val == 0 { 2 } else { 0 };
        new_val
    }

    fn ror(&mut self, val: u8) -> u8 {
        let c = val & 1;
        let new_val = (val >> 1) | ((self.flags & 1) << 7);
        self.flags = (self.flags & 0x7c) | (new_val & 0x80) | c | if new_val == 0 { 2 } else { 0 };
        new_val
    }

    fn adc(&mut self, val: u8) {
        if self.flags & 8 != 0 {
            // decimal mode
            let a = u16::from(self.a);
            let mut sum = a + u16::from(val) + u16::from(self.flags & 1);
            let z = if (sum & 0xff) == 0 { 2 } else { 0 };
            let al = (self.a & 0xf) + (val & 0xf) + (self.flags & 1);
            if al >= 0xa {
                sum += u16::from(((al + 6) & 0xf) + 0x10);
                sum -= u16::from(al);
            }
            let n = (sum as u8) & 0x80;
            let v = (!(self.a ^ val) & (self.a ^ (sum as u8)) & 0x80) >> 1;
            let mut c = 0;
            if sum >= 0xa0 {
                sum += 0x60;
                c = 1;
            }
            self.flags = (self.flags & 0x3c) | n | v | z | c;
            //println!("decimal {:02x} + {:02x} = {:02x}", self.a, val, sum);
            self.a = sum as u8;
        } else {
            let a = u16::from(self.a);
            let sum = a + u16::from(val) + u16::from(self.flags & 1);
            let c = (sum >> 8) as u8;
            let sum = sum as u8;
            let v = (!(self.a ^ val) & (self.a ^ sum) & 0x80) >> 1;
            self.flags = (self.flags & 0x3c) | (sum & 0x80) | c | v | if sum == 0 { 2 } else { 0 };
            self.a = sum;
        }
    }

    fn sbc(&mut self, val: u8) {
        if self.flags & 8 != 0 {
            // decimal mode
            let c_in = self.flags & 1;

            // set flags based on binary computation
            let sum = u16::from(self.a) + u16::from(!val) + u16::from(c_in);
            let c = (sum >> 8) as u8;
            let sum = sum as u8;
            let v = (!(self.a ^ !val) & (self.a ^ sum) & 0x80) >> 1;
            self.flags = (self.flags & 0x3c) | (sum & 0x80) | c | v | if sum == 0 { 2 } else { 0 };

            // do decimal calculation (this follows decimal mode tutorial)
            let mut al = i16::from(self.a & 0xf) - i16::from(val & 0xf) + i16::from(c_in) - 1;
            if al < 0 {
                al = ((al - 6) & 0xf) - 0x10;
            }
            let mut a = i16::from(self.a & 0xf0) - i16::from(val & 0xf0) + al;
            if a < 0 {
                a -= 0x60;
            }
            self.a = a as u8;
        } else {
            self.adc(!val)
        }
    }

    fn cmp(&mut self, lhs: u8, rhs: u8) {
        let sum = u16::from(lhs) + u16::from(!rhs) + 1;
        let c = (sum >> 8) as u8;
        let sum = sum as u8;
        self.flags = (self.flags & 0x7c) | (sum & 0x80) | c | if sum == 0 { 2 } else { 0 };
    }

    // new stuff

    // an adapter, which is probably going away
    pub fn do_bus(&mut self, bus: &mut dyn Bus) {
        if self.is_write {
            bus.write(self.addr, self.data, self);
        } else {
            self.data = bus.read(self.addr, self);
        }
    }

    // logic every clock *after* a bus operation
    pub fn cpu_clk(&mut self) {
        self.insn_cycle = self.insn_cycle + 1;
        println!("insn_cycle = {}, data = {:x}", self.insn_cycle, self.data);
        let insn_cycle = self.insn_cycle;
        if insn_cycle == 1 {
            self.insn = self.data;
            self.pc = self.pc.wrapping_add(1);
            self.addr = self.pc;
            self.is_write = false;
        } else {
            let (addr, is_write) = match self.insn {
                0x00 => {
                    // BRK
                    self.brk()
                }
                0x01 => {
                    // ORA X,ind
                    if insn_cycle == 6 {
                        self.set_a_nz(self.a | self.data);
                    }
                    self.rd_x_ind()
                }
                0x05 => {
                    // ORA zpg
                    if insn_cycle == 3 {
                        self.set_a_nz(self.a | self.data);
                    }
                    self.rd_zpg()
                }
                0x06 => {
                    // ASL zpg
                    if insn_cycle == 4 {
                        self.data = self.asl(self.data);
                    }
                    self.rmw_zpg()
                }
                0x08 => {
                    // PHP
                    if insn_cycle == 2 {
                        self.data = self.flags
                    }
                    self.push()
                }
                0x09 => {
                    // ORA #
                    self.set_a_nz(self.a | self.data);
                    self.rd_imm()
                }
                0x0a => {
                    // ASL A
                    self.a = self.asl(self.a);
                    self.insn_cycle = 0;
                    (self.pc, false)
                }
                0x0d => {
                    // ORA abs
                    if insn_cycle == 4 {
                        self.set_a_nz(self.a | self.data);
                    }
                    self.rd_abs()
                }
                0x0e => {
                    // ASL abs
                    if insn_cycle == 5 {
                        self.data = self.asl(self.data);
                    }
                    self.rmw_abs()
                }
                0x10 => {
                    // BPL rel
                    self.cond_branch((self.flags & 0x80) == 0)
                }
                0x11 => {
                    // ORA ind,Y
                    let (active, next) = self.rd_ind_y();
                    if active {
                        self.set_a_nz(self.a | self.data);
                    }
                    next
                }
                0x15 => {
                    // ORA zpg,X
                    if insn_cycle == 4 {
                        self.set_a_nz(self.a | self.data);
                    }
                    self.rd_zpg_ix(self.x)
                }
                0x16 => {
                    // ASL zpg,X
                    if insn_cycle == 5 {
                        self.data = self.asl(self.data);
                    }
                    self.rmw_zpg_x()
                }
                0x18 => {
                    // CLC
                    self.flags &= !0x01;
                    self.insn_cycle = 0;
                    (self.pc, false)
                }
                0x19 => {
                    // ORA abs,Y
                    let (active, next) = self.rd_abs_ix(self.y);
                    if active {
                        self.set_a_nz(self.a | self.data);
                    }
                    next
                }
                0x1d => {
                    // ORA abs,X
                    let (active, next) = self.rd_abs_ix(self.x);
                    if active {
                        self.set_a_nz(self.a | self.data);
                    }
                    next
                }
                0x1e => {
                    // ASL abs,X
                    if insn_cycle == 6 {
                        self.data = self.asl(self.data);
                    }
                    self.rmw_abs_ix(self.x)
                }
                0x20 => {
                    // JSR abs
                    self.jsr()
                }
                0x21 => {
                    // AND X,ind
                    if insn_cycle == 6 {
                        self.set_a_nz(self.a & self.data);
                    }
                    self.rd_x_ind()
                }
                0x24 => {
                    // BIT zpg
                    if insn_cycle == 3 {
                        self.bit(self.data);
                    }
                    self.rd_zpg()
                }
                0x25 => {
                    // AND zpg
                    if insn_cycle == 3 {
                        self.set_a_nz(self.a & self.data);
                    }
                    self.rd_zpg()
                }
                0x26 => {
                    // ROL zpg
                    if insn_cycle == 4 {
                        self.data = self.rol(self.data);
                    }
                    self.rmw_zpg()
                }
                0x28 => {
                    // PLP
                    if insn_cycle == 4 {
                        self.flags = self.data | 0x30;
                    }
                    self.pull()
                }
                0x29 => {
                    // AND #
                    self.set_a_nz(self.a & self.data);
                    self.rd_imm()
                }
                0x2a => {
                    // ROL A
                    self.a = self.rol(self.a);
                    self.insn_cycle = 0;
                    (self.pc, false)
                }
                0x2c => {
                    // BIT abs
                    if insn_cycle == 4 {
                        self.bit(self.data);
                    }
                    self.rd_abs()
                }
                0x2d => {
                    // AND abs
                    if insn_cycle == 4 {
                        self.set_a_nz(self.a & self.data);
                    }
                    self.rd_abs()
                }
                0x2e => {
                    // ROL abs
                    if insn_cycle == 5 {
                        self.data = self.rol(self.data);
                    }
                    self.rmw_abs()
                }
                0x30 => {
                    // BPL rel
                    self.cond_branch((self.flags & 0x80) != 0)
                }
                0x31 => {
                    // AND ind,Y
                    let (active, next) = self.rd_ind_y();
                    if active {
                        self.set_a_nz(self.a & self.data);
                    }
                    next
                }
                0x35 => {
                    // AND zpg,X
                    if insn_cycle == 4 {
                        self.set_a_nz(self.a & self.data);
                    }
                    self.rd_zpg_ix(self.x)
                }
                0x36 => {
                    // ROL zpg,X
                    if insn_cycle == 5 {
                        self.data = self.rol(self.data);
                    }
                    self.rmw_zpg_x()
                }
                0x38 => {
                    // SEC
                    self.flags |= 0x01;
                    self.insn_cycle = 0;
                    (self.pc, false)
                }
                0x39 => {
                    // AND abs,Y
                    let (active, next) = self.rd_abs_ix(self.y);
                    if active {
                        self.set_a_nz(self.a & self.data);
                    }
                    next
                }
                0x3d => {
                    // AND abs,X
                    let (active, next) = self.rd_abs_ix(self.x);
                    if active {
                        self.set_a_nz(self.a & self.data);
                    }
                    next
                }
                0x3e => {
                    // ROL abs,X
                    if insn_cycle == 6 {
                        self.data = self.rol(self.data);
                    }
                    self.rmw_abs_ix(self.x)
                }
                0x40 => {
                    // RTI
                    self.rti()
                }
                0x41 => {
                    // EOR X,ind
                    if insn_cycle == 6 {
                        self.set_a_nz(self.a ^ self.data);
                    }
                    self.rd_x_ind()
                }
                0x45 => {
                    // EOR zpg
                    if insn_cycle == 3 {
                        self.set_a_nz(self.a ^ self.data);
                    }
                    self.rd_zpg()
                }
                0x46 => {
                    // LSR zpg
                    if insn_cycle == 4 {
                        self.data = self.lsr(self.data);
                    }
                    self.rmw_zpg()
                }
                0x48 => {
                    // PHA
                    if insn_cycle == 2 {
                        self.data = self.a
                    }
                    self.push()
                }
                0x49 => {
                    // EOR #
                    self.set_a_nz(self.a ^ self.data);
                    self.rd_imm()
                }
                0x4a => {
                    // LSR A
                    self.a = self.lsr(self.a);
                    self.insn_cycle = 0;
                    (self.pc, false)
                }
                0x4c => {
                    // JMP abs
                    self.jmp()
                }
                0x4d => {
                    // EOR abs
                    if insn_cycle == 4 {
                        self.set_a_nz(self.a ^ self.data);
                    }
                    self.rd_abs()
                }
                0x4e => {
                    // LSR abs
                    if insn_cycle == 5 {
                        self.data = self.lsr(self.data);
                    }
                    self.rmw_abs()
                }
                0x50 => {
                    // BVC rel
                    self.cond_branch((self.flags & 0x40) == 0)
                }
                0x51 => {
                    // EOR ind,Y
                    let (active, next) = self.rd_ind_y();
                    if active {
                        self.set_a_nz(self.a ^ self.data);
                    }
                    next
                }
                0x55 => {
                    // EOR zpg,X
                    if insn_cycle == 4 {
                        self.set_a_nz(self.a ^ self.data);
                    }
                    self.rd_zpg_ix(self.x)
                }
                0x56 => {
                    // LSR zpg,X
                    if insn_cycle == 5 {
                        self.data = self.lsr(self.data);
                    }
                    self.rmw_zpg_x()
                }
                0x58 => {
                    // CLI
                    self.flags &= !0x04;
                    self.insn_cycle = 0;
                    (self.pc, false)
                }
                0x59 => {
                    // EOR abs,Y
                    let (active, next) = self.rd_abs_ix(self.y);
                    if active {
                        self.set_a_nz(self.a ^ self.data);
                    }
                    next
                }
                0x5d => {
                    // EOR abs,X
                    let (active, next) = self.rd_abs_ix(self.x);
                    if active {
                        self.set_a_nz(self.a ^ self.data);
                    }
                    next
                }
                0x5e => {
                    // LSR abs,X
                    if insn_cycle == 6 {
                        self.data = self.lsr(self.data);
                    }
                    self.rmw_abs_ix(self.x)
                }
                0x60 => {
                    // RTS
                    self.rts()
                }
                0x61 => {
                    // ADC X,ind
                    if insn_cycle == 6 {
                        self.adc(self.data);
                    }
                    self.rd_x_ind()
                }
                0x65 => {
                    // ADC zpg
                    if insn_cycle == 3 {
                        self.adc(self.data);
                    }
                    self.rd_zpg()
                }
                0x66 => {
                    // ROR zpg
                    if insn_cycle == 4 {
                        self.data = self.ror(self.data);
                    }
                    self.rmw_zpg()
                }
                0x68 => {
                    // PLA
                    if insn_cycle == 4 {
                        self.set_a_nz(self.data);
                    }
                    self.pull()
                }
                0x69 => {
                    // ADC #
                    self.adc(self.data);
                    self.rd_imm()
                }
                0x6a => {
                    // ROR A
                    self.a = self.ror(self.a);
                    self.insn_cycle = 0;
                    (self.pc, false)
                }
                0x6c => self.jmp_ind(),
                0x6d => {
                    // ADC abs
                    if insn_cycle == 5 {
                        self.adc(self.data);
                    }
                    self.rmw_abs()
                }
                0x6e => {
                    // ROR abs
                    if insn_cycle == 5 {
                        self.data = self.ror(self.data);
                    }
                    self.rmw_abs()
                }
                0x70 => {
                    // BVS rel
                    self.cond_branch((self.flags & 0x40) != 0)
                }
                0x71 => {
                    // ADC ind,Y
                    let (active, next) = self.rd_ind_y();
                    if active {
                        self.adc(self.data);
                    }
                    next
                }
                0x75 => {
                    // ADC zpg,X
                    if insn_cycle == 4 {
                        self.adc(self.data);
                    }
                    self.rd_zpg_ix(self.x)
                }
                0x76 => {
                    // ROR zpg,X
                    if insn_cycle == 5 {
                        self.data = self.ror(self.data);
                    }
                    self.rmw_zpg_x()
                }
                0x78 => {
                    // SEI
                    self.flags |= 0x04;
                    self.insn_cycle = 0;
                    (self.pc, false)
                }
                0x79 => {
                    // ADC abs,Y
                    let (active, next) = self.rd_abs_ix(self.y);
                    if active {
                        self.adc(self.data);
                    }
                    next
                }
                0x7d => {
                    // ADC abs,X
                    let (active, next) = self.rd_abs_ix(self.x);
                    if active {
                        self.adc(self.data);
                    }
                    next
                }
                0x7e => {
                    // ROR abs,X
                    if insn_cycle == 6 {
                        self.data = self.ror(self.data);
                    }
                    self.rmw_abs_ix(self.x)
                }
                0x81 => {
                    // STA X,ind
                    self.wr_x_ind(self.a)
                }
                0x84 => {
                    // STY zpg
                    self.wr_zpg(self.y)
                }
                0x85 => {
                    // STA zpg
                    self.wr_zpg(self.a)
                }
                0x86 => {
                    // STX zpg
                    self.wr_zpg(self.x)
                }
                0x88 => {
                    // DEY
                    let y = self.y.wrapping_sub(1);
                    self.set_y_nz(y);
                    self.insn_cycle = 0;
                    (self.pc, false)
                }
                0x8a => {
                    // TXA
                    self.set_a_nz(self.x);
                    self.insn_cycle = 0;
                    (self.pc, false)
                }
                0x8c => {
                    // STY abs
                    self.wr_abs(self.y)
                }
                0x8d => {
                    // STA abs
                    self.wr_abs(self.a)
                }
                0x8e => {
                    // STX abs
                    self.wr_abs(self.x)
                }
                0x90 => {
                    // BCC rel
                    self.cond_branch((self.flags & 0x01) == 0)
                }
                0x91 => {
                    // STA ind,Y
                    if insn_cycle == 5 {
                        self.data = self.a;
                    }
                    self.wr_ind_y()
                }
                0x94 => {
                    // STY zpg,X
                    if insn_cycle == 3 {
                        self.data = self.y;
                    }
                    self.wr_zpg_ix(self.x)
                }
                0x95 => {
                    // STA zpg,X
                    if insn_cycle == 3 {
                        self.data = self.a;
                    }
                    self.wr_zpg_ix(self.x)
                }
                0x96 => {
                    // STX zpg,Y
                    if insn_cycle == 3 {
                        self.data = self.x;
                    }
                    self.wr_zpg_ix(self.y)
                }
                0x98 => {
                    // TYA
                    self.set_a_nz(self.y);
                    self.insn_cycle = 0;
                    (self.pc, false)
                }
                0x99 => {
                    // STA abs,Y
                    if insn_cycle == 4 {
                        self.data = self.a;
                    }
                    self.wr_abs_ix(self.y)
                }
                0x9a => {
                    // TXS
                    self.sp = self.x;
                    self.insn_cycle = 0;
                    (self.pc, false)
                }
                0x9d => {
                    // STA abs,X
                    if insn_cycle == 4 {
                        self.data = self.a;
                    }
                    self.wr_abs_ix(self.x)
                }
                0xa0 => {
                    // LDY #
                    self.set_y_nz(self.data);
                    self.rd_imm()
                }
                0xa1 => {
                    // LOAD X,ind
                    if insn_cycle == 6 {
                        self.set_a_nz(self.data);
                    }
                    self.rd_x_ind()
                }
                0xa2 => {
                    // LDX #
                    self.set_x_nz(self.data);
                    self.rd_imm()
                }
                0xa4 => {
                    // LDY zpg
                    if insn_cycle == 3 {
                        self.set_y_nz(self.data);
                    }
                    self.rd_zpg()
                }
                0xa5 => {
                    // LDA zpg
                    if insn_cycle == 3 {
                        self.set_a_nz(self.data);
                    }
                    self.rd_zpg()
                }
                0xa6 => {
                    // LDX zpg
                    if insn_cycle == 3 {
                        self.set_x_nz(self.data);
                    }
                    self.rd_zpg()
                }
                0xa8 => {
                    // TAY
                    self.set_y_nz(self.a);
                    self.insn_cycle = 0;
                    (self.pc, false)
                }
                0xa9 => {
                    // LDA #
                    self.set_a_nz(self.data);
                    self.rd_imm()
                }
                0xaa => {
                    // TAX
                    self.set_x_nz(self.a);
                    self.insn_cycle = 0;
                    (self.pc, false)
                }
                0xac => {
                    // LDY abs
                    if insn_cycle == 4 {
                        self.set_y_nz(self.data);
                    }
                    self.rd_abs()
                }
                0xad => {
                    // LDA abs
                    if insn_cycle == 4 {
                        self.set_a_nz(self.data);
                    }
                    self.rd_abs()
                }
                0xae => {
                    // LDX abs
                    if insn_cycle == 4 {
                        self.set_x_nz(self.data);
                    }
                    self.rd_abs()
                }
                0xb0 => {
                    // BCS rel
                    self.cond_branch((self.flags & 0x01) != 0)
                }
                0xb1 => {
                    // LDA ind,Y
                    let (active, next) = self.rd_ind_y();
                    if active {
                        self.set_a_nz(self.data);
                    }
                    next
                }
                0xb4 => {
                    // LDY zpg,X
                    if insn_cycle == 4 {
                        self.set_y_nz(self.data);
                    }
                    self.rd_zpg_ix(self.x)
                }
                0xb5 => {
                    // LDA zpg,X
                    if insn_cycle == 4 {
                        self.set_a_nz(self.data);
                    }
                    self.rd_zpg_ix(self.x)
                }
                0xb6 => {
                    // LDX zpg,Y
                    if insn_cycle == 4 {
                        self.set_x_nz(self.data);
                    }
                    self.rd_zpg_ix(self.y)
                }
                0xb8 => {
                    // CLV
                    self.flags &= !0x40;
                    self.insn_cycle = 0;
                    (self.pc, false)
                }
                0xb9 => {
                    // LDA abs,Y
                    let (active, next) = self.rd_abs_ix(self.y);
                    if active {
                        self.set_a_nz(self.data);
                    }
                    next
                }
                0xba => {
                    // TSX
                    self.set_x_nz(self.sp);
                    self.insn_cycle = 0;
                    (self.pc, false)
                }
                0xbc => {
                    // LDY abs,X
                    let (active, next) = self.rd_abs_ix(self.x);
                    if active {
                        self.set_y_nz(self.data);
                    }
                    next
                }
                0xbd => {
                    // LDA abs,X
                    let (active, next) = self.rd_abs_ix(self.x);
                    if active {
                        self.set_a_nz(self.data);
                    }
                    next
                }
                0xbe => {
                    // LDX abs,Y
                    let (active, next) = self.rd_abs_ix(self.y);
                    if active {
                        self.set_x_nz(self.data);
                    }
                    next
                }
                0xc0 => {
                    // CPY #
                    self.cmp(self.y, self.data);
                    self.rd_imm()
                }
                0xc1 => {
                    // CMP X,ind
                    if insn_cycle == 6 {
                        self.cmp(self.a, self.data);
                    }
                    self.rd_x_ind()
                }
                0xc4 => {
                    // CPY zpg
                    if insn_cycle == 3 {
                        self.cmp(self.y, self.data);
                    }
                    self.rd_zpg()
                }
                0xc5 => {
                    // CMP zpg
                    if insn_cycle == 3 {
                        self.cmp(self.a, self.data);
                    }
                    self.rd_zpg()
                }
                0xc6 => {
                    // DEC zpg
                    if insn_cycle == 4 {
                        let val = self.data.wrapping_sub(1);
                        self.data = val;
                        self.set_nz(val);
                    }
                    self.rmw_zpg()
                }
                0xc8 => {
                    // INY
                    let y = self.y.wrapping_add(1);
                    self.set_y_nz(y);
                    self.insn_cycle = 0;
                    (self.pc, false)
                }
                0xc9 => {
                    // CMP #
                    self.cmp(self.a, self.data);
                    self.rd_imm()
                }
                0xca => {
                    // DEX
                    let x = self.x.wrapping_sub(1);
                    self.set_x_nz(x);
                    self.insn_cycle = 0;
                    (self.pc, false)
                }
                0xcc => {
                    // CPY abs
                    if insn_cycle == 4 {
                        self.cmp(self.y, self.data);
                    }
                    self.rd_abs()
                }
                0xcd => {
                    // CMP abs
                    if insn_cycle == 4 {
                        self.cmp(self.a, self.data);
                    }
                    self.rd_abs()
                }
                0xce => {
                    // DEC abs
                    if insn_cycle == 5 {
                        let val = self.data.wrapping_sub(1);
                        self.data = val;
                        self.set_nz(val);
                    }
                    self.rmw_abs()
                }
                0xd0 => {
                    // BNE rel
                    self.cond_branch((self.flags & 0x02) == 0)
                }
                0xd1 => {
                    // CMP ind,Y
                    let (active, next) = self.rd_ind_y();
                    if active {
                        self.cmp(self.a, self.data);
                    }
                    next
                }
                0xd5 => {
                    // CMP zpg,X
                    if insn_cycle == 4 {
                        self.cmp(self.a, self.data);
                    }
                    self.rd_zpg_ix(self.x)
                }
                0xd6 => {
                    // DEC zpg,X
                    if insn_cycle == 5 {
                        let val = self.data.wrapping_sub(1);
                        self.data = val;
                        self.set_nz(val);
                    }
                    self.rmw_zpg_x()
                }
                0xd8 => {
                    // CLD
                    self.flags &= !0x08;
                    self.insn_cycle = 0;
                    (self.pc, false)
                }
                0xd9 => {
                    // CMP abs,Y
                    let (active, next) = self.rd_abs_ix(self.y);
                    if active {
                        self.cmp(self.a, self.data);
                    }
                    next
                }
                0xdd => {
                    // CMP abs,X
                    let (active, next) = self.rd_abs_ix(self.x);
                    if active {
                        self.cmp(self.a, self.data);
                    }
                    next
                }
                0xde => {
                    // DEC abs,X
                    if insn_cycle == 6 {
                        let val = self.data.wrapping_sub(1);
                        self.data = val;
                        self.set_nz(val);
                    }
                    self.rmw_abs_ix(self.x)
                }
                0xe0 => {
                    // CPX #
                    self.cmp(self.x, self.data);
                    self.rd_imm()
                }
                0xe1 => {
                    // SBC X,ind
                    if insn_cycle == 6 {
                        self.sbc(self.data);
                    }
                    self.rd_x_ind()
                }
                0xe4 => {
                    // CPX zpg
                    if insn_cycle == 3 {
                        self.cmp(self.x, self.data);
                    }
                    self.rd_zpg()
                }
                0xe5 => {
                    // SBC zpg
                    if insn_cycle == 3 {
                        self.sbc(self.data);
                    }
                    self.rd_zpg()
                }
                0xe6 => {
                    // INC zpg
                    if insn_cycle == 4 {
                        let val = self.data.wrapping_add(1);
                        self.data = val;
                        self.set_nz(val);
                    }
                    self.rmw_zpg()
                }
                0xe8 => {
                    // INX
                    self.set_x_nz(self.x.wrapping_add(1));
                    self.insn_cycle = 0;
                    (self.pc, false)
                }
                0xe9 => {
                    // SBC #
                    self.sbc(self.data);
                    self.rd_imm()
                }
                0xea => {
                    // NOP
                    self.insn_cycle = 0;
                    (self.pc, false)
                }
                0xec => {
                    // CPX abs
                    if insn_cycle == 4 {
                        self.cmp(self.x, self.data);
                    }
                    self.rd_abs()
                }
                0xed => {
                    // SBC abs
                    if insn_cycle == 5 {
                        self.sbc(self.data);
                    }
                    self.rmw_abs()
                }
                0xee => {
                    // INC abs
                    if insn_cycle == 5 {
                        let val = self.data.wrapping_add(1);
                        self.data = val;
                        self.set_nz(val);
                    }
                    self.rmw_abs()
                }
                0xf0 => {
                    // BEQ rel
                    self.cond_branch((self.flags & 0x02) != 0)
                }
                0xf1 => {
                    // SBC ind,Y
                    let (active, next) = self.rd_ind_y();
                    if active {
                        self.sbc(self.data);
                    }
                    next
                }
                0xf5 => {
                    // SBC zpg,X
                    if insn_cycle == 4 {
                        self.sbc(self.data);
                    }
                    self.rd_zpg_ix(self.x)
                }
                0xf6 => {
                    // INC zpg,X
                    if insn_cycle == 5 {
                        let val = self.data.wrapping_add(1);
                        self.data = val;
                        self.set_nz(val);
                    }
                    self.rmw_zpg_x()
                }
                0xf8 => {
                    // SED
                    self.flags |= 0x08;
                    self.insn_cycle = 0;
                    (self.pc, false)
                }
                0xf9 => {
                    // SBC abs,Y
                    let (active, next) = self.rd_abs_ix(self.y);
                    if active {
                        self.sbc(self.data);
                    }
                    next
                }
                0xfd => {
                    // SBC abs,X
                    let (active, next) = self.rd_abs_ix(self.x);
                    if active {
                        self.sbc(self.data);
                    }
                    next
                }
                0xfe => {
                    // INC abs,X
                    if insn_cycle == 6 {
                        let val = self.data.wrapping_add(1);
                        self.data = val;
                        self.set_nz(val);
                    }
                    self.rmw_abs_ix(self.x)
                }
                _ => panic!("unhandled insn {:x}", self.insn),
            };
            self.addr = addr;
            self.is_write = is_write;
        }
    }

    fn rd_x_ind(&mut self) -> (u16, bool) {
        if self.insn_cycle == 2 {
            self.pc = self.pc.wrapping_add(1);
            self.ptr = self.data;
            (self.ptr as u16, false)
        } else if self.insn_cycle == 3 {
            self.ptr = self.ptr.wrapping_add(self.x);
            (self.ptr as u16, false)
        } else if self.insn_cycle == 4 {
            self.lo = self.data;
            let addr = self.ptr.wrapping_add(1);
            (addr as u16, false)
        } else if self.insn_cycle == 5 {
            let addr = ((self.data as u16) << 8) | (self.lo as u16);
            (addr, false)
        } else {
            self.insn_cycle = 0;
            (self.pc, false)
        }
    }

    fn wr_x_ind(&mut self, val: u8) -> (u16, bool) {
        if self.insn_cycle == 2 {
            self.pc = self.pc.wrapping_add(1);
            self.ptr = self.data;
            (self.ptr as u16, false)
        } else if self.insn_cycle == 3 {
            self.ptr = self.ptr.wrapping_add(self.x);
            (self.ptr as u16, false)
        } else if self.insn_cycle == 4 {
            self.lo = self.data;
            let addr = self.ptr.wrapping_add(1);
            (addr as u16, false)
        } else if self.insn_cycle == 5 {
            let addr = ((self.data as u16) << 8) | (self.lo as u16);
            self.data = val;
            (addr, true)
        } else {
            self.insn_cycle = 0;
            (self.pc, false)
        }
    }

    fn rd_zpg(&mut self) -> (u16, bool) {
        if self.insn_cycle == 2 {
            self.pc = self.pc.wrapping_add(1);
            (self.data as u16, false)
        } else {
            self.insn_cycle = 0;
            (self.pc, false)
        }
    }

    fn wr_zpg(&mut self, val: u8) -> (u16, bool) {
        if self.insn_cycle == 2 {
            self.pc = self.pc.wrapping_add(1);
            let addr = self.data as u16;
            self.data = val;
            (addr, true)
        } else {
            self.insn_cycle = 0;
            (self.pc, false)
        }
    }

    fn rd_zpg_ix(&mut self, ix: u8) -> (u16, bool) {
        if self.insn_cycle == 2 {
            self.pc = self.pc.wrapping_add(1);
            self.ptr = self.data;
            (self.ptr as u16, false)
        } else if self.insn_cycle == 3 {
            let addr = self.ptr.wrapping_add(ix);
            (addr as u16, false)
        } else {
            self.insn_cycle = 0;
            (self.pc, false)
        }
    }

    fn wr_zpg_ix(&mut self, ix: u8) -> (u16, bool) {
        if self.insn_cycle == 2 {
            self.pc = self.pc.wrapping_add(1);
            self.ptr = self.data;
            (self.ptr as u16, false)
        } else if self.insn_cycle == 3 {
            let addr = self.ptr.wrapping_add(ix);
            (addr as u16, true)
        } else {
            self.insn_cycle = 0;
            (self.pc, false)
        }
    }

    fn rd_abs(&mut self) -> (u16, bool) {
        if self.insn_cycle == 2 {
            self.pc = self.pc.wrapping_add(1);
            self.lo = self.data;
            (self.pc as u16, false)
        } else if self.insn_cycle == 3 {
            self.pc = self.pc.wrapping_add(1);
            let addr = ((self.data as u16) << 8) | (self.lo as u16);
            (addr, false)
        } else {
            self.insn_cycle = 0;
            (self.pc, false)
        }
    }

    fn wr_abs(&mut self, val: u8) -> (u16, bool) {
        if self.insn_cycle == 2 {
            self.pc = self.pc.wrapping_add(1);
            self.lo = self.data;
            (self.pc as u16, false)
        } else if self.insn_cycle == 3 {
            self.pc = self.pc.wrapping_add(1);
            let addr = ((self.data as u16) << 8) | (self.lo as u16);
            self.data = val;
            (addr, true)
        } else {
            self.insn_cycle = 0;
            (self.pc, false)
        }
    }

    fn rd_abs_ix(&mut self, ix: u8) -> (bool, (u16, bool)) {
        if self.insn_cycle == 2 {
            self.pc = self.pc.wrapping_add(1);
            self.lo = self.data;
            (false, (self.pc, false))
        } else if self.insn_cycle == 3 {
            self.pc = self.pc.wrapping_add(1);
            let lo = self.lo.wrapping_add(ix);
            let addr = ((self.data as u16) << 8) | (lo as u16);
            (false, (addr, false))
        } else if self.insn_cycle == 4 {
            if self.lo.overflowing_add(ix).1 {
                let addr = self.addr.wrapping_add(0x100);
                (false, (addr, false))
            } else {
                self.insn_cycle = 0;
                (true, (self.pc, false))
            }
        } else {
            self.insn_cycle = 0;
            (true, (self.pc, false))
        }
    }

    fn wr_abs_ix(&mut self, ix: u8) -> (u16, bool) {
        if self.insn_cycle == 2 {
            self.pc = self.pc.wrapping_add(1);
            self.lo = self.data;
            (self.pc, false)
        } else if self.insn_cycle == 3 {
            self.pc = self.pc.wrapping_add(1);
            let lo = self.lo.wrapping_add(ix);
            let addr = ((self.data as u16) << 8) | (lo as u16);
            (addr, false)
        } else if self.insn_cycle == 4 {
            if self.lo.overflowing_add(ix).1 {
                self.addr = self.addr.wrapping_add(0x100);
            }
            (self.addr, true)
        } else {
            self.insn_cycle = 0;
            (self.pc, false)
        }
    }

    fn rd_ind_y(&mut self) -> (bool, (u16, bool)) {
        if self.insn_cycle == 2 {
            self.pc = self.pc.wrapping_add(1);
            self.ptr = self.data;
            (false, (self.ptr as u16, false))
        } else if self.insn_cycle == 3 {
            self.lo = self.data;
            (false, (self.ptr.wrapping_add(1) as u16, false))
        } else if self.insn_cycle == 4 {
            let lo = self.lo.wrapping_add(self.y);
            let addr = ((self.data as u16) << 8) | (lo as u16);
            (false, (addr, false))
        } else if self.insn_cycle == 5 {
            if self.lo.overflowing_add(self.y).1 {
                let addr = self.addr.wrapping_add(0x100);
                (false, (addr, false))
            } else {
                self.insn_cycle = 0;
                (true, (self.pc, false))
            }
        } else {
            self.insn_cycle = 0;
            (true, (self.pc, false))
        }
    }

    fn wr_ind_y(&mut self) -> (u16, bool) {
        if self.insn_cycle == 2 {
            self.pc = self.pc.wrapping_add(1);
            self.ptr = self.data;
            (self.ptr as u16, false)
        } else if self.insn_cycle == 3 {
            self.lo = self.data;
            (self.ptr.wrapping_add(1) as u16, false)
        } else if self.insn_cycle == 4 {
            let lo = self.lo.wrapping_add(self.y);
            let addr = ((self.data as u16) << 8) | (lo as u16);
            (addr, false)
        } else if self.insn_cycle == 5 {
            if self.lo.overflowing_add(self.y).1 {
                self.addr = self.addr.wrapping_add(0x100);
            }
            (self.addr, true)
        } else {
            self.insn_cycle = 0;
            (self.pc, false)
        }
    }

    fn rmw_abs(&mut self) -> (u16, bool) {
        if self.insn_cycle == 2 {
            self.pc = self.pc.wrapping_add(1);
            self.lo = self.data;
            (self.pc as u16, false)
        } else if self.insn_cycle == 3 {
            self.pc = self.pc.wrapping_add(1);
            let addr = ((self.data as u16) << 8) | (self.lo as u16);
            (addr, false)
        } else if self.insn_cycle == 4 {
            (self.addr, true)
        } else if self.insn_cycle == 5 {
            (self.addr, true)
        } else {
            self.insn_cycle = 0;
            (self.pc, false)
        }
    }

    fn rmw_abs_ix(&mut self, ix: u8) -> (u16, bool) {
        if self.insn_cycle == 2 {
            self.pc = self.pc.wrapping_add(1);
            self.lo = self.data;
            (self.pc as u16, false)
        } else if self.insn_cycle == 3 {
            self.pc = self.pc.wrapping_add(1);
            let lo = self.lo.wrapping_add(ix);
            let addr = ((self.data as u16) << 8) | (lo as u16);
            (addr, false)
        } else if self.insn_cycle == 4 {
            if self.lo.overflowing_add(ix).1 {
                self.addr = self.addr.wrapping_add(0x100);
            }
            (self.addr, false)
        } else if self.insn_cycle == 5 {
            (self.addr, true)
        } else if self.insn_cycle == 6 {
            (self.addr, true)
        } else {
            self.insn_cycle = 0;
            (self.pc, false)
        }
    }

    fn rmw_zpg(&mut self) -> (u16, bool) {
        if self.insn_cycle == 2 {
            self.pc = self.pc.wrapping_add(1);
            self.ptr = self.data;
            (self.ptr as u16, false)
        } else if self.insn_cycle == 3 {
            (self.ptr as u16, true)
        } else if self.insn_cycle == 4 {
            (self.ptr as u16, true)
        } else {
            self.insn_cycle = 0;
            (self.pc, false)
        }
    }

    fn rmw_zpg_x(&mut self) -> (u16, bool) {
        if self.insn_cycle == 2 {
            self.pc = self.pc.wrapping_add(1);
            self.ptr = self.data;
            (self.ptr as u16, false)
        } else if self.insn_cycle == 3 {
            let addr = self.ptr.wrapping_add(self.x);
            (addr as u16, false)
        } else if self.insn_cycle == 4 {
            (self.addr as u16, true)
        } else if self.insn_cycle == 5 {
            (self.addr as u16, true)
        } else {
            self.insn_cycle = 0;
            (self.pc, false)
        }
    }

    fn rd_imm(&mut self) -> (u16, bool) {
        self.pc = self.pc.wrapping_add(1);
        self.insn_cycle = 0;
        (self.pc, false)
    }

    fn push(&mut self) -> (u16, bool) {
        if self.insn_cycle == 2 {
            let addr = 0x100 | (self.sp as u16);
            self.sp = self.sp.wrapping_sub(1);
            (addr, true)
        } else {
            self.insn_cycle = 0;
            (self.pc, false)
        }
    }

    fn pull(&mut self) -> (u16, bool) {
        if self.insn_cycle == 2 {
            let addr = 0x100 | (self.sp as u16);
            self.sp = self.sp.wrapping_add(1);
            (addr, false)
        } else if self.insn_cycle == 3 {
            let addr = 0x100 | (self.sp as u16);
            (addr, false)
        } else {
            self.insn_cycle = 0;
            (self.pc, false)
        }
    }

    fn cond_branch(&mut self, cond: bool) -> (u16, bool) {
        if self.insn_cycle == 2 {
            self.pc = self.pc.wrapping_add(1);
            if cond {
                self.lo = self.data;
            } else {
                self.insn_cycle = 0;
            }
            (self.pc, false)
        } else if self.insn_cycle == 3 {
            let new_pc = self.pc.wrapping_add(self.lo as i8 as u16);
            let nocarry_pc = (self.pc & 0xff00) | (new_pc & 0xff);
            self.pc = new_pc;
            if new_pc == nocarry_pc {
                self.insn_cycle = 0;
            }
            (nocarry_pc, false)
        } else {
            self.insn_cycle = 0;
            (self.pc, false)
        }
    }

    fn jmp(&mut self) -> (u16, bool) {
        if self.insn_cycle == 2 {
            self.pc = self.pc.wrapping_add(1);
            self.lo = self.data;
            (self.pc, false)
        } else {
            self.pc = ((self.data as u16) << 8) | (self.lo as u16);
            self.insn_cycle = 0;
            (self.pc, false)
        }
    }

    fn jmp_ind(&mut self) -> (u16, bool) {
        if self.insn_cycle == 2 {
            self.pc = self.pc.wrapping_add(1);
            self.lo = self.data;
            (self.pc, false)
        } else if self.insn_cycle == 3 {
            let addr = ((self.data as u16) << 8) | (self.lo as u16);
            (addr, false)
        } else if self.insn_cycle == 4 {
            self.lo = self.data;
            // 64doc says this only wraps lo byte
            let addr = self.addr.wrapping_add(1);
            (addr, false)
        } else {
            self.pc = ((self.data as u16) << 8) | (self.lo as u16);
            self.insn_cycle = 0;
            (self.pc, false)
        }
    }

    fn jsr(&mut self) -> (u16, bool) {
        if self.insn_cycle == 2 {
            self.pc = self.pc.wrapping_add(1);
            self.lo = self.data;
            let addr = 0x100 | (self.sp as u16);
            (addr, false)
        } else if self.insn_cycle == 3 {
            self.data = (self.pc >> 8) as u8;
            self.sp = self.sp.wrapping_sub(1);
            (self.addr, true)
        } else if self.insn_cycle == 4 {
            self.data = self.pc as u8;
            let addr = 0x100 | (self.sp as u16);
            self.sp = self.sp.wrapping_sub(1);
            (addr, true)
        } else if self.insn_cycle == 5 {
            (self.pc, false)
        } else {
            self.pc = ((self.data as u16) << 8) | (self.lo as u16);
            self.insn_cycle = 0;
            (self.pc, false)
        }
    }

    fn rts(&mut self) -> (u16, bool) {
        if self.insn_cycle == 2 {
            let addr = 0x100 | (self.sp as u16);
            self.sp = self.sp.wrapping_add(1);
            (addr, false)
        } else if self.insn_cycle == 3 {
            let addr = 0x100 | (self.sp as u16);
            self.sp = self.sp.wrapping_add(1);
            (addr, false)
        } else if self.insn_cycle == 4 {
            self.lo = self.data;
            let addr = 0x100 | (self.sp as u16);
            (addr, false)
        } else if self.insn_cycle == 5 {
            self.pc = ((self.data as u16) << 8) | (self.lo as u16);
            (self.pc, false)
        } else {
            self.pc = self.pc.wrapping_add(1);
            self.insn_cycle = 0;
            (self.pc, false)
        }
    }

    fn rti(&mut self) -> (u16, bool) {
        if self.insn_cycle == 2 {
            let addr = 0x100 | (self.sp as u16);
            self.sp = self.sp.wrapping_add(1);
            (addr, false)
        } else if self.insn_cycle == 3 {
            let addr = 0x100 | (self.sp as u16);
            self.sp = self.sp.wrapping_add(1);
            (addr, false)
        } else if self.insn_cycle == 4 {
            self.flags = self.data;
            let addr = 0x100 | (self.sp as u16);
            self.sp = self.sp.wrapping_add(1);
            (addr, false)
        } else if self.insn_cycle == 5 {
            self.lo = self.data;
            let addr = 0x100 | (self.sp as u16);
            (addr, false)
        } else {
            self.pc = ((self.data as u16) << 8) | (self.lo as u16);
            self.insn_cycle = 0;
            (self.pc, false)
        }
    }

    fn brk(&mut self) -> (u16, bool) {
        if self.insn_cycle == 2 {
            self.pc = self.pc.wrapping_add(1);
            self.data = (self.pc >> 8) as u8;
            let addr = 0x100 | (self.sp as u16);
            self.sp = self.sp.wrapping_sub(1);
            (addr, true)
        } else if self.insn_cycle == 3 {
            self.data = self.pc as u8;
            let addr = 0x100 | (self.sp as u16);
            self.sp = self.sp.wrapping_sub(1);
            (addr, true)
        } else if self.insn_cycle == 4 {
            self.data = self.flags;
            let addr = 0x100 | (self.sp as u16);
            self.sp = self.sp.wrapping_sub(1);
            (addr, true)
        } else if self.insn_cycle == 5 {
            (0xfffe, false)
        } else if self.insn_cycle == 6 {
            self.lo = self.data;
            (0xffff, false)
        } else {
            self.pc = ((self.data as u16) << 8) | (self.lo as u16);
            self.flags |= 0x04;
            self.insn_cycle = 0;
            (self.pc, false)
        }
    }
}
