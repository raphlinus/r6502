
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
            flags: 0x30,
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

    pub fn print_state(&self) {
        println!("a {:02x} x {:02x} y {:02x} s {:02x} p {:02x} pc {:04x}",
            self.a, self.x, self.y, self.sp, self.flags, self.pc);
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

    fn zp_read(&mut self, addr: u8, bus: &mut Bus) -> u8 {
        /*
        let result = self.mem[usize::from(addr)];
        self.cycle += 1;
        result
        */
        self.read(u16::from(addr), bus)
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
            0x00 => { // BRK
                self.brk(bus);
            }
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
                self.a = new_val;
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
                self.flags = self.pull(bus) | 0x30;
            }
            0x29 => { // AND #
                let a = self.a & self.imm(bus);
                self.set_a_nz(a);
            }
            0x2a => { // ROL A
                let a = self.a;
                let new_val = self.rol(a);
                self.a = new_val;
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

            0x40 => { // RTI
                self.rti(bus);
            }
            0x41 => { // EOR X,ind
                let a = self.a ^ self.read_x_ind(bus);
                self.set_a_nz(a);
            }
            0x45 => { // EOR zpg
                let a = self.a ^ self.read_zp(bus);
                self.set_a_nz(a);
            }
            0x46 => { // LSR zpg
                let (val, addr) = self.rd_wr_zp(bus);
                let new_val = self.lsr(val);
                self.zp_write(addr, new_val, bus);
            }
            0x48 => { // PHA
                let a = self.a;
                self.push(a, bus);
            }
            0x49 => { // EOR #
                let a = self.a ^ self.imm(bus);
                self.set_a_nz(a);
            }
            0x4a => { // LSR A
                let a = self.a;
                let new_val = self.lsr(a);
                self.a = new_val;
            }
            0x4c => { // JMP abs
                let addr = self.abs_addr(bus);
                self.pc = addr;
            }
            0x4d => { // EOR abs
                let a = self.a ^ self.read_abs(bus);
                self.set_a_nz(a);
            }
            0x4e => { // LSR abs
                let (val, addr) = self.rd_wr_abs(bus);
                let new_val = self.lsr(val);
                self.write(addr, new_val, bus);
            }

            0x50 => { // BVC rel
                let cond = (self.flags & 0x40) == 0;
                self.cond_branch(cond, bus);
            }
            0x51 => { // EOR ind,Y
                let a = self.a ^ self.read_ind_y(bus);
                self.set_a_nz(a);
            }
            0x55 => { // EOR zpg,X
                let a = self.a ^ self.read_zp_x(bus);
                self.set_a_nz(a);
            }
            0x56 => { // LSR zpg,X
                let (val, addr) = self.rd_wr_zp_x(bus);
                let new_val = self.lsr(val);
                self.zp_write(addr, new_val, bus);
            }
            0x58 => { // CLI
                self.flags &= !0x04;
                self.waste_cycle(bus);
            }
            0x59 => { // EOR abs,Y
                let a = self.a ^ self.read_abs_y(bus);
                self.set_a_nz(a);
            }
            0x5d => { // EOR abs,X
                let a = self.a ^ self.read_abs_x(bus);
                self.set_a_nz(a);
            }
            0x5e => { // LSR abs,X
                let (val, addr) = self.rd_wr_abs_x(bus);
                let new_val = self.lsr(val);
                self.write(addr, new_val, bus);
            }

            0x60 => { // RTS
                self.rts(bus);
            }
            0x61 => { // ADC X,ind
                let val = self.read_x_ind(bus);
                self.adc(val);
            }
            0x65 => { // ADC zpg
                let val = self.read_zp(bus);
                self.adc(val);
            }
            0x66 => { // ROR zpg
                let (val, addr) = self.rd_wr_zp(bus);
                let new_val = self.ror(val);
                self.zp_write(addr, new_val, bus);
            }
            0x68 => { // PLA
                let a = self.pull(bus);
                self.set_a_nz(a);
            }
            0x69 => { // ADC #
                let val = self.imm(bus);
                self.adc(val);
            }
            0x6a => { // ROR A
                let a = self.a;
                let new_val = self.ror(a);
                self.set_a_nz(new_val);
            }
            0x6c => { // JMP ind
                let addr = self.abs_addr(bus);
                let lo = self.read(addr, bus);
                let hi = self.read(addr.wrapping_add(1), bus);
                self.pc = (u16::from(hi) << 8) | u16::from(lo);
            }
            0x6d => { // ADC abs
                let val = self.read_abs(bus);
                self.adc(val);
            }
            0x6e => { // ROR abs
                let (val, addr) = self.rd_wr_abs(bus);
                let new_val = self.ror(val);
                self.write(addr, new_val, bus);
            }

            0x70 => { // BVS rel
                let cond = (self.flags & 0x40) != 0;
                self.cond_branch(cond, bus);
            }
            0x71 => { // ADC X,ind,Y
                let val = self.read_ind_y(bus);
                self.adc(val);
            }
            0x75 => { // ADC zpg,X
                let val = self.read_zp_x(bus);
                self.adc(val);
            }
            0x76 => { // ROR zpg,X
                let (val, addr) = self.rd_wr_zp_x(bus);
                let new_val = self.ror(val);
                self.zp_write(addr, new_val, bus);
            }
            0x78 => { // SEI
                self.flags |= 0x04;
                self.waste_cycle(bus);
            }
            0x79 => { // ADC abs,Y
                let val = self.read_abs_y(bus);
                self.adc(val);
            }
            0x7d => { // ADC abs,X
                let val = self.read_abs_x(bus);
                self.adc(val);
            }
            0x7e => { // ROR abs,X
                let (val, addr) = self.rd_wr_abs_x(bus);
                let new_val = self.ror(val);
                self.write(addr, new_val, bus);
            }

            0x81 => { // STA X,ind
                let a = self.a;
                self.write_x_ind(a, bus);
            }
            0x84 => { // STY zpg
                let y = self.y;
                self.write_zp(y, bus);
            }
            0x85 => { // STA zpg
                let a = self.a;
                self.write_zp(a, bus);
            }
            0x86 => { // STX zpg
                let x = self.x;
                self.write_zp(x, bus);
            }
            0x88 => { // DEY
                let y = self.y.wrapping_sub(1);
                self.set_y_nz(y);
                self.waste_cycle(bus);
            }
            0x8a => { // TXA
                let x = self.x;
                self.set_a_nz(x);
                self.waste_cycle(bus);
            }
            0x8c => { // STY abs
                let y = self.y;
                self.write_abs(y, bus);
            }
            0x8d => { // STA abs
                let a = self.a;
                self.write_abs(a, bus);
            }
            0x8e => { // STX abs
                let x = self.x;
                self.write_abs(x, bus);
            }

            0x90 => { // BCC rel
                let cond = (self.flags & 0x01) == 0;
                self.cond_branch(cond, bus);
            }
            0x91 => { // STA ind,Y
                let a = self.a;
                self.write_ind_y(a, bus);
            }
            0x94 => { // STY zpg,X
                let y = self.y;
                self.write_zp_x(y, bus);
            }
            0x95 => { // STA zpg,X
                let a = self.a;
                self.write_zp_x(a, bus);
            }
            0x96 => { // STX zpg,Y
                let x = self.x;
                self.write_zp_y(x, bus);
            }
            0x98 => { // TYA
                let y = self.y;
                self.set_a_nz(y);
                self.waste_cycle(bus);
            }
            0x99 => { // STA abs,Y
                let a = self.a;
                self.write_abs_y(a, bus);
            }
            0x9a => { // TXS
                self.sp = self.x;
                self.waste_cycle(bus);
            }
            0x9d => { // STA abs,X
                let a = self.a;
                self.write_abs_x(a, bus);
            }

            0xa0 => { // LDY #
                let y = self.imm(bus);
                self.set_y_nz(y);
            }
            0xa1 => { // LDA X,ind
                let a = self.read_x_ind(bus);
                self.set_a_nz(a);
            }
            0xa2 => { // LDX #
                let x = self.imm(bus);
                self.set_x_nz(x);
            }
            0xa4 => { // LDY zpg
                let y = self.read_zp(bus);
                self.set_y_nz(y);
            }
            0xa5 => { // LDA zpg
                let a = self.read_zp(bus);
                self.set_a_nz(a);
            }
            0xa6 => { // LDX zpg
                let x = self.read_zp(bus);
                self.set_x_nz(x);
            }
            0xa8 => { // TAY
                let a = self.a;
                self.set_y_nz(a);
                self.waste_cycle(bus);
            }
            0xa9 => { // LDA #
                let a = self.imm(bus);
                self.set_a_nz(a);
            }
            0xaa => { // TAX
                let a = self.a;
                self.set_x_nz(a);
                self.waste_cycle(bus);
            }
            0xac => { // LDY abs
                let y = self.read_abs(bus);
                self.set_y_nz(y);
            }
            0xad => { // LDA abs
                let a = self.read_abs(bus);
                self.set_a_nz(a);
            }
            0xae => { // LDX abs
                let x = self.read_abs(bus);
                self.set_x_nz(x);
            }

            0xb0 => { // BCS rel
                let cond = (self.flags & 0x01) != 0;
                self.cond_branch(cond, bus);
            }
            0xb1 => { // LDA ind,Y
                let a = self.read_ind_y(bus);
                self.set_a_nz(a);
            }
            0xb4 => { // LDY zpg,X
                let y = self.read_zp_x(bus);
                self.set_y_nz(y);
            }
            0xb5 => { // LDA zpg,X
                let a = self.read_zp_x(bus);
                self.set_a_nz(a);
            }
            0xb6 => { // LDX zpg,Y
                let x = self.read_zp_y(bus);
                self.set_x_nz(x);
            }
            0xb8 => { // CLV
                self.flags &= !0x40;
                self.waste_cycle(bus);
            }
            0xb9 => { // LDA abs,Y
                let a = self.read_abs_y(bus);
                self.set_a_nz(a);
            }
            0xba => { // TSX
                let sp = self.sp;
                self.set_x_nz(sp);
                self.waste_cycle(bus);
            }
            0xbc => { // LDY abs,X
                let y = self.read_abs_x(bus);
                self.set_y_nz(y);
            }
            0xbd => { // LDA abs,X
                let a = self.read_abs_x(bus);
                self.set_a_nz(a);
            }
            0xbe => { // LDX abs,Y
                let x = self.read_abs_y(bus);
                self.set_x_nz(x);
            }

            0xc0 => { // CPY #
                let y = self.y;
                let rhs = self.imm(bus);
                self.cmp(y, rhs);
            }
            0xc1 => { // CMP X,ind
                let a = self.a;
                let rhs = self.read_x_ind(bus);
                self.cmp(a, rhs);
            }
            0xc4 => { // CPY zpg
                let y = self.y;
                let rhs = self.read_zp(bus);
                self.cmp(y, rhs);
            }
            0xc5 => { // CMP zpg
                let a = self.a;
                let rhs = self.read_zp(bus);
                self.cmp(a, rhs);
            }
            0xc6 => { // DEC zpg
                let (val, addr) = self.rd_wr_zp(bus);
                let val = val.wrapping_sub(1);
                self.zp_write(addr, val, bus);
                self.set_nz(val);
            }
            0xc8 => { // INY
                let y = self.y.wrapping_add(1);
                self.set_y_nz(y);
                self.waste_cycle(bus);
            }
            0xc9 => { // CMP #
                let a = self.a;
                let rhs = self.imm(bus);
                self.cmp(a, rhs);
            }
            0xca => { // DEX
                let x = self.x.wrapping_sub(1);
                self.set_x_nz(x);
                self.waste_cycle(bus);
            }
            0xcc => { // CPY abs
                let y = self.y;
                let rhs = self.read_abs(bus);
                self.cmp(y, rhs);
            }
            0xcd => { // CMP abs
                let a = self.a;
                let rhs = self.read_abs(bus);
                self.cmp(a, rhs);
            }
            0xce => { // DEC abs
                let (val, addr) = self.rd_wr_abs(bus);
                let val = val.wrapping_sub(1);
                self.write(addr, val, bus);
                self.set_nz(val);
            }

            0xd0 => { // BNE rel
                let cond = (self.flags & 0x02) == 0;
                self.cond_branch(cond, bus);
            }
            0xd1 => { // CMP ind,Y
                let a = self.a;
                let rhs = self.read_ind_y(bus);
                self.cmp(a, rhs);
            }
            0xd5 => { // CMP zpg,X
                let a = self.a;
                let rhs = self.read_zp_x(bus);
                self.cmp(a, rhs);
            }
            0xd6 => { // DEC zpg,X
                let (val, addr) = self.rd_wr_zp_x(bus);
                let val = val.wrapping_sub(1);
                self.zp_write(addr, val, bus);
                self.set_nz(val);
            }
            0xd8 => { // CLD
                self.flags &= !0x08;
                self.waste_cycle(bus);
            }
            0xd9 => { // CMP abs,Y
                let a = self.a;
                let rhs = self.read_abs_y(bus);
                self.cmp(a, rhs);
            }
            0xdd => { // CMP abs,X
                let a = self.a;
                let rhs = self.read_abs_x(bus);
                self.cmp(a, rhs);
            }
            0xde => { // DEC abs,X
                let (val, addr) = self.rd_wr_abs_x(bus);
                let val = val.wrapping_sub(1);
                self.write(addr, val, bus);
                self.set_nz(val);
            }

            0xe0 => { // CPX #
                let x = self.x;
                let rhs = self.imm(bus);
                self.cmp(x, rhs);
            }
            0xe1 => { // SBC X,ind
                let val = self.read_x_ind(bus);
                self.sbc(val);
            }
            0xe4 => { // CPX zpg
                let x = self.x;
                let rhs = self.read_zp(bus);
                self.cmp(x, rhs);
            }
            0xe5 => { // SBC zpg
                let val = self.read_zp(bus);
                self.sbc(val);
            }
            0xe6 => { // INC zpg
                let (val, addr) = self.rd_wr_zp(bus);
                let val = val.wrapping_add(1);
                self.zp_write(addr, val, bus);
                self.set_nz(val);
            }
            0xe8 => { // INX
                let x = self.x.wrapping_add(1);
                self.set_x_nz(x);
                self.waste_cycle(bus);
            }
            0xe9 => { // SBC #
                let val = self.imm(bus);
                self.sbc(val);
            }
            0xea => { // NOP
                self.waste_cycle(bus);
            }
            0xec => { // CPX abs
                let x = self.x;
                let rhs = self.read_abs(bus);
                self.cmp(x, rhs);
            }
            0xed => { // SBC abs
                let val = self.read_abs(bus);
                self.sbc(val);
            }
            0xee => { // INC abs
                let (val, addr) = self.rd_wr_abs(bus);
                let val = val.wrapping_add(1);
                self.write(addr, val, bus);
                self.set_nz(val);
            }

            0xf0 => { // BEQ rel
                let cond = (self.flags & 0x02) != 0;
                self.cond_branch(cond, bus);
            }
            0xf1 => { // SBC X,ind,Y
                let val = self.read_ind_y(bus);
                self.sbc(val);
            }
            0xf5 => { // SBC zpg,X
                let val = self.read_zp_x(bus);
                self.sbc(val);
            }
            0xf6 => { // INC zpg,X
                let (val, addr) = self.rd_wr_zp_x(bus);
                let val = val.wrapping_add(1);
                self.zp_write(addr, val, bus);
                self.set_nz(val);
            }
            0xf8 => { // SED
                self.flags |= 0x08;
                self.waste_cycle(bus);
            }
            0xf9 => { // SBC abs,Y
                let val = self.read_abs_y(bus);
                self.sbc(val);
            }
            0xfd => { // SBC abs,X
                let val = self.read_abs_x(bus);
                self.sbc(val);
            }
            0xfe => { // INC abs,X
                let (val, addr) = self.rd_wr_abs_x(bus);
                let val = val.wrapping_add(1);
                self.write(addr, val, bus);
                self.set_nz(val);
            }

            _ => println!("unimpl ins {:02x}", ins),
        }
    }

    fn write_stack(&mut self, val: u8, bus: &mut Bus) {
        let addr = 0x100 | u16::from(self.sp);
        self.write(addr, val, bus);
    }

    fn push(&mut self, val: u8, bus: &mut Bus) {
        self.waste_cycle(bus);
        self.write_stack(val, bus);
        self.sp = self.sp.wrapping_sub(1);
    }

    fn read_stack(&mut self, bus: &mut Bus) -> u8 {
        let addr = 0x100 | u16::from(self.sp);
        self.read(addr, bus)
    }

    fn pull(&mut self, bus: &mut Bus) -> u8 {
        self.waste_cycle(bus);
        let _ = self.read_stack(bus);
        self.sp = self.sp.wrapping_add(1);
        self.read_stack(bus)
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
        let _ = self.read_stack(bus);
        let hi = (self.pc >> 8) as u8;
        self.write_stack(hi, bus);
        self.sp = self.sp.wrapping_sub(1);
        let lo = self.pc as u8;
        self.write_stack(lo, bus);
        self.sp = self.sp.wrapping_sub(1);
        let target_hi = self.imm(bus);
        self.pc = (u16::from(target_hi) << 8) | u16::from(target_lo);
    }

    fn rts(&mut self, bus: &mut Bus) {
        self.waste_cycle(bus);
        let _ = self.read_stack(bus);
        self.sp = self.sp.wrapping_add(1);
        let lo = self.read_stack(bus);
        self.sp = self.sp.wrapping_add(1);
        let hi = self.read_stack(bus);
        let pc = (u16::from(hi) << 8) | u16::from(lo);
        let _ = self.read(pc, bus);
        self.pc = pc.wrapping_add(1);
    }

    fn brk(&mut self, bus: &mut Bus) {
        self.waste_cycle(bus);
        self.pc = self.pc.wrapping_add(1);
        let hi = (self.pc >> 8) as u8;
        self.write_stack(hi, bus);
        self.sp = self.sp.wrapping_sub(1);
        let lo = self.pc as u8;
        self.write_stack(lo, bus);
        self.sp = self.sp.wrapping_sub(1);
        let flags = self.flags;
        self.write_stack(flags, bus);
        self.sp = self.sp.wrapping_sub(1);
        let lo = self.read(0xfffe, bus);
        let hi = self.read(0xffff, bus);
        self.pc = (u16::from(hi) << 8) | u16::from(lo);
        self.flags |= 0x04;
    }

    fn rti(&mut self, bus: &mut Bus) {
        self.waste_cycle(bus);
        let _ = self.read_stack(bus);
        self.sp = self.sp.wrapping_add(1);
        self.flags = self.read_stack(bus);
        self.sp = self.sp.wrapping_add(1);
        let lo = self.read_stack(bus);
        self.sp = self.sp.wrapping_add(1);
        let hi = self.read_stack(bus);
        self.pc = (u16::from(hi) << 8) | u16::from(lo);
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
        let lo = self.zp_read(addr, bus);
        let hi = self.zp_read(addr.wrapping_add(1), bus);
        self.read((u16::from(hi) << 8) | u16::from(lo), bus)
    }

    fn write_x_ind(&mut self, val: u8, bus: &mut Bus) {
        let addr = self.zp_x_addr(bus);
        let lo = self.zp_read(addr, bus);
        let hi = self.zp_read(addr.wrapping_add(1), bus);
        self.write((u16::from(hi) << 8) | u16::from(lo), val, bus);
    }

    fn ind_y_addr(&mut self, bus: &mut Bus) -> (u16, u16) {
        let addr = self.imm(bus);
        let lo = self.zp_read(addr, bus);
        let hi = self.zp_read(addr.wrapping_add(1), bus);
        let addr = (u16::from(hi) << 8) | u16::from(lo);
        let new_addr = addr.wrapping_add(u16::from(self.y));
        let addr_no_carry = (addr & 0xff00) | (new_addr & 0xff);
        (addr_no_carry, new_addr)        
    }

    fn read_ind_y(&mut self, bus: &mut Bus) -> u8 {
        let (addr_no_carry, new_addr) = self.ind_y_addr(bus);
        if new_addr != addr_no_carry {
            let _ = self.read(addr_no_carry, bus);
        }
        self.read(new_addr, bus)
    }

    fn write_ind_y(&mut self, val: u8, bus: &mut Bus) {
        let (addr_no_carry, new_addr) = self.ind_y_addr(bus);
        let _ = self.read(addr_no_carry, bus);
        self.write(new_addr, val, bus);
    }

    fn zp_addr(&mut self, bus: &mut Bus) -> u8 {
        self.imm(bus)
    }

    fn read_zp(&mut self, bus: &mut Bus) -> u8 {
        let addr = self.zp_addr(bus);
        self.zp_read(addr, bus)
    }

    fn write_zp(&mut self, val: u8, bus: &mut Bus) {
        let addr = self.zp_addr(bus);
        self.zp_write(addr, val, bus);
    }

    fn rd_wr_zp(&mut self, bus: &mut Bus) -> (u8, u8) {
        let addr = self.zp_addr(bus);
        let val = self.zp_read(addr, bus);
        self.zp_write(addr, val, bus);  // wasted cycle
        (val, addr)
    }

    fn zp_x_addr(&mut self, bus: &mut Bus) -> u8 {
        let addr = self.zp_addr(bus);
        let _ = self.zp_read(addr, bus);
        addr.wrapping_add(self.x)
    }

    fn read_zp_x(&mut self, bus: &mut Bus) -> u8 {
        let addr = self.zp_x_addr(bus);
        self.zp_read(addr, bus)
    }

    fn write_zp_x(&mut self, val: u8, bus: &mut Bus) {
        let addr = self.zp_x_addr(bus);
        self.zp_write(addr, val, bus);
    }

    fn rd_wr_zp_x(&mut self, bus: &mut Bus) -> (u8, u8) {
        let addr = self.zp_x_addr(bus);
        let val = self.zp_read(addr, bus);
        self.zp_write(addr, val, bus);  // wasted cycle
        (val, addr)
    }

    fn zp_y_addr(&mut self, bus: &mut Bus) -> u8 {
        let addr = self.zp_addr(bus);
        let _ = self.zp_read(addr, bus);
        addr.wrapping_add(self.y)
    }

    fn read_zp_y(&mut self, bus: &mut Bus) -> u8 {
        let addr = self.zp_y_addr(bus);
        self.zp_read(addr, bus)
    }

    fn write_zp_y(&mut self, val: u8, bus: &mut Bus) {
        let addr = self.zp_y_addr(bus);
        self.zp_write(addr, val, bus);
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

    fn write_abs_ix(&mut self, val: u8, ix: u8, bus: &mut Bus) {
        let addr = self.abs_addr(bus);
        let new_addr = addr.wrapping_add(u16::from(ix));
        let addr_no_carry = (addr & 0xff00) | (new_addr & 0xff);
        let _ = self.read(addr_no_carry, bus);
        self.write(new_addr, val, bus);
    }

    fn write_abs_x(&mut self, val: u8, bus: &mut Bus) {
        let x = self.x;
        self.write_abs_ix(val, x, bus);
    }

    fn write_abs_y(&mut self, val: u8, bus: &mut Bus) {
        let y = self.y;
        self.write_abs_ix(val, y, bus);
    }

    fn rd_wr_abs_x(&mut self, bus: &mut Bus) -> (u8, u16) {
        let addr = self.abs_addr(bus);
        let new_addr = addr.wrapping_add(u16::from(self.x));
        let addr_no_carry = (addr & 0xff00) | (new_addr & 0xff);
        let _ = self.read(addr_no_carry, bus);
        let val = self.read(new_addr, bus);
        self.write(new_addr, val, bus);  // wasted cycle
        (val, new_addr)
    }

    fn read_abs_y(&mut self, bus: &mut Bus) -> u8 {
        let addr = self.abs_y_addr(bus);
        self.read(addr, bus)
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

    fn lsr(&mut self, val: u8) -> u8 {
        let c = val & 1;
        let new_val = val >> 1;
        self.flags = (self.flags & 0x7c) | (new_val & 0x80) | c
            | if new_val == 0 { 2 } else { 0 };
        new_val
    }

    fn ror(&mut self, val: u8) -> u8 {
        let c = val & 1;
        let new_val = (val >> 1) | ((self.flags & 1) << 7);
        self.flags = (self.flags & 0x7c) | (new_val & 0x80) | c
            | if new_val == 0 { 2 } else { 0 };
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
            self.flags = (self.flags & 0x3c) | (sum & 0x80) | c | v
                | if sum == 0 { 2 } else { 0 };
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
            self.flags = (self.flags & 0x3c) | (sum & 0x80) | c | v
                | if sum == 0 { 2 } else { 0 };

            // do decimal calculation (this follows decimal mode tutorial)
            let mut al = i16::from(self.a & 0xf) - i16::from(val & 0xf)
                + i16::from(c_in) - 1;
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
        self.flags = (self.flags & 0x7c) | (sum & 0x80) | c
            | if sum == 0 { 2 } else { 0 };
    }
}
