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
    pub cycle: usize,
}

#[repr(u8)]
#[derive(Clone, Copy, Debug)]
enum UcodeStep {
    Next,
    Fetch,
    // Immediate ALU ops
    OraImm,
    AndImm,
    EorImm,
    AdcImm,
    LdaImm,
    LdxImm,
    LdyImm,
    CpxImm,
    CpyImm,
    CmpImm,
    SbcImm,
    // Memory ALU ops
    Ora,
    And,
    Eor,
    Adc,
    Lda,
    Ldx,
    Ldy,
    Cmp,
    Cpx,
    Cpy,
    Bit,
    Sbc,
    // Memory RMW ops
    Rmw2,
    AslMem,
    RolMem,
    LsrMem,
    RorMem,
    IncMem,
    DecMem,
    // Single cycle ops
    AslA,
    RolA,
    LsrA,
    RorA,
    Clc,
    Sec,
    Cli,
    Sei,
    Cld,
    Sed,
    Clv,
    Txs,
    Tsx,
    Tax,
    Tay,
    Txa,
    Tya,
    // Addressing modes
    Abs1,
    Abs2,
    AbsX,
    AbsY,
    AbsIx,
    Zpg,
    ZpgX,
    ZpgY,
    Ind,
    IndY2,
    IndY3,
    XInd2,
    XInd4,
    OraIx,
    AndIx,
    EorIx,
    LdaIx,
    LdxIx,
    LdyIx,
    AdcIx,
    CmpIx,
    SbcIx,
    StaAbs,
    StxAbs,
    StyAbs,
    StaAbsIx,
    StaZpg,
    StxZpg,
    StyZpg,
    StaZpgX,
    StxZpgY,
    StyZpgX,
    StaXInd,
    Jmp,
    Bpl,
    Bmi,
    Bvc,
    Bvs,
    Bne,
    Beq,
    Bcc,
    Bcs,
    CondBr,
    Dex,
    Dey,
    Inx,
    Iny,
    // Stack ops
    Pha,
    Php,
    Pull1,
    Pull2,
    Plp,
    Jsr1,
    Jsr2,
    Brk1,
    Jsr3,
    Jsr4,
    Rts3,
    Rts4,
    Rts5,
    Rti3,
    Brk3,
    Brk4,
    Brk5,
    Brk6,
    Nyi,
}

use UcodeStep::*;

const fn c1(s1: UcodeStep) -> [UcodeStep; 8] {
    [Fetch, s1, Next, Next, Next, Next, Next, Next]
}

const fn c2(s1: UcodeStep, s2: UcodeStep) -> [UcodeStep; 8] {
    [Fetch, s1, s2, Next, Next, Next, Next, Next]
}

const fn c3(s1: UcodeStep, s2: UcodeStep, s3: UcodeStep) -> [UcodeStep; 8] {
    [Fetch, s1, s2, s3, Next, Next, Next, Next]
}

const fn c4(s1: UcodeStep, s2: UcodeStep, s3: UcodeStep, s4: UcodeStep) -> [UcodeStep; 8] {
    [Fetch, s1, s2, s3, s4, Next, Next, Next]
}

const fn c5(
    s1: UcodeStep,
    s2: UcodeStep,
    s3: UcodeStep,
    s4: UcodeStep,
    s5: UcodeStep,
) -> [UcodeStep; 8] {
    [Fetch, s1, s2, s3, s4, s5, Next, Next]
}

const fn c6(
    s1: UcodeStep,
    s2: UcodeStep,
    s3: UcodeStep,
    s4: UcodeStep,
    s5: UcodeStep,
    s6: UcodeStep,
) -> [UcodeStep; 8] {
    [Fetch, s1, s2, s3, s4, s5, s6, Next]
}

const UCODE: [[UcodeStep; 8]; 256] = [
    c6(Brk1, Jsr3, Brk3, Brk4, Brk5, Brk6),    // 00 BRK
    c5(Zpg, XInd2, IndY2, XInd4, Ora),         // 01 ORA X,ind
    c1(Nyi),                                   // 02
    c1(Nyi),                                   // 03
    c1(Nyi),                                   // 04
    c2(Zpg, Ora),                              // 05 ORA zpg
    c4(Zpg, Rmw2, AslMem, Next),               // 06 ASL zpg
    c1(Nyi),                                   // 07
    c2(Php, Next),                             // 08 PHP
    c1(OraImm),                                // 09 ORA #
    c1(AslA),                                  // 0a ASL A
    c1(Nyi),                                   // 0b
    c1(Nyi),                                   // 0c
    c3(Abs1, Abs2, Ora),                       // 0d ORA abs
    c5(Abs1, Abs2, Rmw2, AslMem, Next),        // 0e ASL abs
    c1(Nyi),                                   // 0f
    c3(Bpl, CondBr, Next),                     // 10 BPL rel
    c5(Zpg, IndY2, IndY3, OraIx, Ora),         // 11 ORA ind,y
    c1(Nyi),                                   // 12
    c1(Nyi),                                   // 13
    c1(Nyi),                                   // 14
    c3(Zpg, ZpgX, Ora),                        // 15 ORA zpg,X
    c5(Zpg, ZpgX, Rmw2, AslMem, Next),         // 16 ASL zpg,X
    c1(Nyi),                                   // 17
    c1(Clc),                                   // 18 CLC
    c4(Abs1, AbsY, OraIx, Ora),                // 19 ORA abs,Y
    c1(Nyi),                                   // 1a
    c1(Nyi),                                   // 1b
    c1(Nyi),                                   // 1c
    c4(Abs1, AbsX, OraIx, Ora),                // 1d ORA abs,X
    c6(Abs1, AbsX, AbsIx, Rmw2, AslMem, Next), // 1e ASL abs,X
    c1(Nyi),                                   // 1f
    c5(Jsr1, Jsr2, Jsr3, Jsr4, Jmp),           // 20 JSR
    c5(Zpg, XInd2, IndY2, XInd4, And),         // 21 AND X,ind
    c1(Nyi),                                   // 22
    c1(Nyi),                                   // 23
    c2(Zpg, Bit),                              // 24 BIT zpg
    c2(Zpg, And),                              // 25 AND zpg
    c4(Zpg, Rmw2, RolMem, Next),               // 26 ROL zpg
    c1(Nyi),                                   // 27
    c3(Pull1, Pull2, Plp),                     // 28 PLP
    c1(AndImm),                                // 29 AND #
    c1(RolA),                                  // 2a ROL A
    c1(Nyi),                                   // 2b
    c3(Abs1, Abs2, Bit),                       // 2c BIT abs
    c3(Abs1, Abs2, And),                       // 2d AND abs
    c5(Abs1, Abs2, Rmw2, RolMem, Next),        // 2e ROL abs
    c1(Nyi),                                   // 2f
    c3(Bmi, CondBr, Next),                     // 30 BMI rel
    c5(Zpg, IndY2, IndY3, AndIx, And),         // 31 AND ind,y
    c1(Nyi),                                   // 32
    c1(Nyi),                                   // 33
    c1(Nyi),                                   // 34
    c3(Zpg, ZpgX, And),                        // 35 AND zpg,X
    c5(Zpg, ZpgX, Rmw2, RolMem, Next),         // 36 ROL zpg,X
    c1(Nyi),                                   // 37
    c1(Sec),                                   // 38 SEC
    c4(Abs1, AbsY, AndIx, And),                // 39 AND abs,Y
    c1(Nyi),                                   // 3a
    c1(Nyi),                                   // 3b
    c1(Nyi),                                   // 3c
    c4(Abs1, AbsX, AndIx, And),                // 3d AND abs,X
    c6(Abs1, AbsX, AbsIx, Rmw2, RolMem, Next), // 3e ROL abs,X
    c1(Nyi),                                   // 3f
    c5(Pull1, Pull1, Rti3, Rts3, Jmp),         // 40 RTI
    c5(Zpg, XInd2, IndY2, XInd4, Eor),         // 41 EOR X,ind
    c1(Nyi),                                   // 42
    c1(Nyi),                                   // 43
    c1(Nyi),                                   // 44
    c2(Zpg, Eor),                              // 45 EOR zpg
    c4(Zpg, Rmw2, LsrMem, Next),               // 46 LSR zpg
    c1(Nyi),                                   // 47
    c2(Pha, Next),                             // 48 PHA
    c1(EorImm),                                // 49 EOR #
    c1(LsrA),                                  // 4a LSR A
    c1(Nyi),                                   // 4b
    c2(Abs1, Jmp),                             // 4c JMP abs
    c3(Abs1, Abs2, Eor),                       // 4d EOR abs
    c5(Abs1, Abs2, Rmw2, LsrMem, Next),        // 4e LSR abs
    c1(Nyi),                                   // 4f
    c3(Bvc, CondBr, Next),                     // 50 BVC rel
    c5(Zpg, IndY2, IndY3, EorIx, Eor),         // 51 EOR ind,y
    c1(Nyi),                                   // 52
    c1(Nyi),                                   // 53
    c1(Nyi),                                   // 54
    c3(Zpg, ZpgX, Eor),                        // 55 EOR zpg,X
    c5(Zpg, ZpgX, Rmw2, LsrMem, Next),         // 56 LSR zpg,X
    c1(Nyi),                                   // 57
    c1(Cli),                                   // 58 CLI
    c4(Abs1, AbsY, EorIx, Eor),                // 59 EOR abs,Y
    c1(Nyi),                                   // 5a
    c1(Nyi),                                   // 5b
    c1(Nyi),                                   // 5c
    c4(Abs1, AbsX, EorIx, Eor),                // 5d EOR abs,X
    c6(Abs1, AbsX, AbsIx, Rmw2, LsrMem, Next), // 5e LSR abs,X
    c1(Nyi),                                   // 5f
    c5(Pull1, Pull1, Rts3, Rts4, Rts5),        // 60 RTS
    c5(Zpg, XInd2, IndY2, XInd4, Adc),         // 61 ADC X,ind
    c1(Nyi),                                   // 62
    c1(Nyi),                                   // 63
    c1(Nyi),                                   // 64
    c2(Zpg, Adc),                              // 65 ADC zpg
    c4(Zpg, Rmw2, RorMem, Next),               // 66 ROR zpg
    c1(Nyi),                                   // 67
    c3(Pull1, Pull2, Lda),                     // 68 PLA
    c1(AdcImm),                                // 69 ADC #
    c1(RorA),                                  // 6a ROR A
    c1(Nyi),                                   // 6b
    c4(Abs1, Abs2, Ind, Jmp),                  // 6c JMP ind
    c3(Abs1, Abs2, Adc),                       // 6d ADC abs
    c5(Abs1, Abs2, Rmw2, RorMem, Next),        // 6e ROR abs
    c1(Nyi),                                   // 6f
    c3(Bvs, CondBr, Next),                     // 70 BVS rel
    c5(Zpg, IndY2, IndY3, AdcIx, Adc),         // 71 ADC ind,y
    c1(Nyi),                                   // 72
    c1(Nyi),                                   // 73
    c1(Nyi),                                   // 74
    c3(Zpg, ZpgX, Adc),                        // 75 ADC zpg,X
    c5(Zpg, ZpgX, Rmw2, RorMem, Next),         // 76 ROR zpg,X
    c1(Nyi),                                   // 77
    c1(Sei),                                   // 78 SEI
    c4(Abs1, AbsY, AdcIx, Adc),                // 79 ADC abs,Y
    c1(Nyi),                                   // 7a
    c1(Nyi),                                   // 7b
    c1(Nyi),                                   // 7c
    c4(Abs1, AbsX, AdcIx, Adc),                // 7d ADC abs,X
    c6(Abs1, AbsX, AbsIx, Rmw2, RorMem, Next), // 7e ROR abs,X
    c1(Nyi),                                   // 7f
    c1(Nyi),                                   // 80
    c5(Zpg, XInd2, IndY2, StaXInd, Next),      // 81 STA X,ind
    c1(Nyi),                                   // 82
    c1(Nyi),                                   // 83
    c2(StyZpg, Next),                          // 84 STY zpg
    c2(StaZpg, Next),                          // 85 STA zpg
    c2(StxZpg, Next),                          // 86 STX zpg
    c1(Nyi),                                   // 87
    c1(Dey),                                   // 88 DEY
    c1(Nyi),                                   // 89
    c1(Txa),                                   // 8a TXA
    c1(Nyi),                                   // 8b
    c3(Abs1, StyAbs, Next),                    // 8c STY abs
    c3(Abs1, StaAbs, Next),                    // 8d STA abs
    c3(Abs1, StxAbs, Next),                    // 8e STX abs
    c1(Nyi),                                   // 8f
    c3(Bcc, CondBr, Next),                     // 90 BCC rel
    c5(Zpg, IndY2, IndY3, StaAbsIx, Next),     // 91 STA ind,Y
    c1(Nyi),                                   // 92
    c1(Nyi),                                   // 93
    c3(Zpg, StyZpgX, Next),                    // 94 STY zpg,X
    c3(Zpg, StaZpgX, Next),                    // 95 STA zpg,X
    c3(Zpg, StxZpgY, Next),                    // 96 STX zpg,Y
    c1(Nyi),                                   // 97
    c1(Tya),                                   // 98 TYA
    c4(Abs1, AbsY, StaAbsIx, Next),            // 99 STA abs,Y
    c1(Txs),                                   // 9a TXS
    c1(Nyi),                                   // 9b
    c1(Nyi),                                   // 9c
    c4(Abs1, AbsX, StaAbsIx, Next),            // 9d STA abs,X
    c1(Nyi),                                   // 9e
    c1(Nyi),                                   // 9f
    c1(LdyImm),                                // a0 LDY #
    c5(Zpg, XInd2, IndY2, XInd4, Lda),         // a1 LDA X,ind
    c1(LdxImm),                                // a2 LDX #
    c1(Nyi),                                   // a3
    c2(Zpg, Ldy),                              // a4 LDY zpg
    c2(Zpg, Lda),                              // a5 LDA zpg
    c2(Zpg, Ldx),                              // a6 LDX zpg
    c1(Nyi),                                   // a7
    c1(Tay),                                   // a8 TAY
    c1(LdaImm),                                // a9 LDA #
    c1(Tax),                                   // aa TAX
    c1(Nyi),                                   // ab
    c3(Abs1, Abs2, Ldy),                       // ac LDY abs
    c3(Abs1, Abs2, Lda),                       // ad LDA abs
    c3(Abs1, Abs2, Ldx),                       // ae LDX abs
    c1(Nyi),                                   // af
    c3(Bcs, CondBr, Next),                     // b0 BCS rel
    c5(Zpg, IndY2, IndY3, LdaIx, Lda),         // b1 LDA ind,y
    c1(Nyi),                                   // b2
    c1(Nyi),                                   // b3
    c3(Zpg, ZpgX, Ldy),                        // b4 LDY zpg,X
    c3(Zpg, ZpgX, Lda),                        // b5 LDA zpg,X
    c3(Zpg, ZpgY, Ldx),                        // b6 LDX zpg,Y
    c1(Nyi),                                   // b7
    c1(Clv),                                   // b8 CLV
    c4(Abs1, AbsY, LdaIx, Lda),                // b9 LDA abs,Y
    c1(Tsx),                                   // ba TSX
    c1(Nyi),                                   // bb
    c4(Abs1, AbsX, LdyIx, Ldy),                // bc LDY abs,X
    c4(Abs1, AbsX, LdaIx, Lda),                // bd LDA abs,X
    c4(Abs1, AbsY, LdxIx, Ldx),                // be LDX abs,Y
    c1(Nyi),                                   // bf
    c1(CpyImm),                                // c0 CPY #
    c5(Zpg, XInd2, IndY2, XInd4, Cmp),         // c1 CMP X,ind
    c1(Nyi),                                   // c2
    c1(Nyi),                                   // c3
    c2(Zpg, Cpy),                              // c4 CPY zpg
    c2(Zpg, Cmp),                              // c5 CMP zpg
    c4(Zpg, Rmw2, DecMem, Next),               // c6 DEC zpg
    c1(Nyi),                                   // c7
    c1(Iny),                                   // c8 INY
    c1(CmpImm),                                // c9 CMP #
    c1(Dex),                                   // ca DEX
    c1(Nyi),                                   // cb
    c3(Abs1, Abs2, Cpy),                       // cc CPY abs
    c3(Abs1, Abs2, Cmp),                       // cd CMP abs
    c5(Abs1, Abs2, Rmw2, DecMem, Next),        // ce DEC abs
    c1(Nyi),                                   // cf
    c3(Bne, CondBr, Next),                     // d0 BNE rel
    c5(Zpg, IndY2, IndY3, CmpIx, Cmp),         // d1 CMP ind,y
    c1(Nyi),                                   // d2
    c1(Nyi),                                   // d3
    c1(Nyi),                                   // d4
    c3(Zpg, ZpgX, Cmp),                        // d5 CMP zpg,X
    c5(Zpg, ZpgX, Rmw2, DecMem, Next),         // d6 DEC zpg,X
    c1(Nyi),                                   // d7
    c1(Cld),                                   // d8 CLD
    c4(Abs1, AbsY, CmpIx, Cmp),                // d9 CMP abs,Y
    c1(Nyi),                                   // da
    c1(Nyi),                                   // db
    c1(Nyi),                                   // dc
    c4(Abs1, AbsX, CmpIx, Cmp),                // dd CMP abs,X
    c6(Abs1, AbsX, AbsIx, Rmw2, DecMem, Next), // de DEC abs,X
    c1(Nyi),                                   // df
    c1(CpxImm),                                // e0 CPX #
    c5(Zpg, XInd2, IndY2, XInd4, Sbc),         // e1 SBC X,ind
    c1(Nyi),                                   // e2
    c1(Nyi),                                   // e3
    c2(Zpg, Cpx),                              // e4 CPX zpg
    c2(Zpg, Sbc),                              // e5 SBC zpg
    c4(Zpg, Rmw2, IncMem, Next),               // e6 INC zpg
    c1(Nyi),                                   // e7
    c1(Inx),                                   // e8 INX
    c1(SbcImm),                                // e9 SBC #
    c1(Next),                                  // ea NOP
    c1(Nyi),                                   // eb
    c3(Abs1, Abs2, Cpx),                       // ec CPX abs
    c3(Abs1, Abs2, Sbc),                       // ed SBC abs
    c5(Abs1, Abs2, Rmw2, IncMem, Next),        // ee INC abs
    c1(Nyi),                                   // ef
    c3(Beq, CondBr, Next),                     // f0 BEQ rel
    c5(Zpg, IndY2, IndY3, SbcIx, Sbc),         // f1 SBC ind,y
    c1(Nyi),                                   // f2
    c1(Nyi),                                   // f3
    c1(Nyi),                                   // f4
    c3(Zpg, ZpgX, Sbc),                        // f5 SBC zpg,X
    c5(Zpg, ZpgX, Rmw2, IncMem, Next),         // f6 INC zpg,X
    c1(Nyi),                                   // f7
    c1(Sed),                                   // f8 SED
    c4(Abs1, AbsY, SbcIx, Sbc),                // f9 SBC abs,Y
    c1(Nyi),                                   // fa
    c1(Nyi),                                   // fb
    c1(Nyi),                                   // fc
    c4(Abs1, AbsX, SbcIx, Sbc),                // fd SBC abs,X
    c6(Abs1, AbsX, AbsIx, Rmw2, IncMem, Next), // fe INC abs,X
    c1(Nyi),                                   // ff
];

pub trait Bus {
    fn read(&mut self, addr: u16) -> u8;

    fn write(&mut self, addr: u16, val: u8);
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

            cycle: 0,
        }
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
            bus.write(self.addr, self.data);
        } else {
            self.data = bus.read(self.addr);
        }
    }

    pub fn cpu_clk_ucode(&mut self) {
        let insn_cycle = self.insn_cycle;
        self.insn_cycle += 1;
        let ucode = UCODE[self.insn as usize][insn_cycle as usize];
        println!("{:x} {insn_cycle} {ucode:?}", self.insn);
        let (addr, is_write) = match ucode {
            Fetch => {
                self.insn = self.data;
                self.pc = self.pc.wrapping_add(1);
                (self.pc, false)
            }
            Next => {
                self.insn_cycle = 0;
                (self.pc, false)
            }
            AslA => {
                self.a = self.asl(self.a);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            RolA => {
                self.a = self.rol(self.a);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            LsrA => {
                self.a = self.lsr(self.a);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            RorA => {
                self.a = self.ror(self.a);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            Clc => {
                self.flags &= !0x01;
                self.insn_cycle = 0;
                (self.pc, false)
            }
            Sec => {
                self.flags |= 0x01;
                self.insn_cycle = 0;
                (self.pc, false)
            }
            Cli => {
                self.flags &= !0x04;
                self.insn_cycle = 0;
                (self.pc, false)
            }
            Sei => {
                self.flags |= 0x04;
                self.insn_cycle = 0;
                (self.pc, false)
            }
            Cld => {
                self.flags &= !0x08;
                self.insn_cycle = 0;
                (self.pc, false)
            }
            Sed => {
                self.flags |= 0x08;
                self.insn_cycle = 0;
                (self.pc, false)
            }
            Clv => {
                self.flags &= !0x40;
                self.insn_cycle = 0;
                (self.pc, false)
            }
            OraImm => {
                self.set_a_nz(self.a | self.data);
                self.pc = self.pc.wrapping_add(1);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            AndImm => {
                self.set_a_nz(self.a & self.data);
                self.pc = self.pc.wrapping_add(1);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            EorImm => {
                self.set_a_nz(self.a ^ self.data);
                self.pc = self.pc.wrapping_add(1);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            AdcImm => {
                self.adc(self.data);
                self.pc = self.pc.wrapping_add(1);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            LdaImm => {
                self.set_a_nz(self.data);
                self.pc = self.pc.wrapping_add(1);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            LdxImm => {
                self.set_x_nz(self.data);
                self.pc = self.pc.wrapping_add(1);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            LdyImm => {
                self.set_y_nz(self.data);
                self.pc = self.pc.wrapping_add(1);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            CmpImm => {
                self.cmp(self.a, self.data);
                self.pc = self.pc.wrapping_add(1);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            CpxImm => {
                self.cmp(self.x, self.data);
                self.pc = self.pc.wrapping_add(1);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            CpyImm => {
                self.cmp(self.y, self.data);
                self.pc = self.pc.wrapping_add(1);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            SbcImm => {
                self.sbc(self.data);
                self.pc = self.pc.wrapping_add(1);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            Ora => {
                self.set_a_nz(self.a | self.data);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            And => {
                self.set_a_nz(self.a & self.data);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            Eor => {
                self.set_a_nz(self.a ^ self.data);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            Adc => {
                self.adc(self.data);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            Lda => {
                self.set_a_nz(self.data);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            Ldx => {
                self.set_x_nz(self.data);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            Ldy => {
                self.set_y_nz(self.data);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            Cmp => {
                self.cmp(self.a, self.data);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            Cpx => {
                self.cmp(self.x, self.data);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            Cpy => {
                self.cmp(self.y, self.data);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            Bit => {
                self.bit(self.data);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            Sbc => {
                self.sbc(self.data);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            Rmw2 => (self.addr, true),
            AslMem => {
                self.data = self.asl(self.data);
                (self.addr, true)
            }
            RolMem => {
                self.data = self.rol(self.data);
                (self.addr, true)
            }
            LsrMem => {
                self.data = self.lsr(self.data);
                (self.addr, true)
            }
            RorMem => {
                self.data = self.ror(self.data);
                (self.addr, true)
            }
            IncMem => {
                let val = self.data.wrapping_add(1);
                self.data = val;
                self.set_nz(val);
                (self.addr, true)
            }
            DecMem => {
                let val = self.data.wrapping_sub(1);
                self.data = val;
                self.set_nz(val);
                (self.addr, true)
            }
            Abs1 => {
                self.pc = self.pc.wrapping_add(1);
                self.lo = self.data;
                (self.pc, false)
            }
            Abs2 => {
                self.pc = self.pc.wrapping_add(1);
                let addr = ((self.data as u16) << 8) | (self.lo as u16);
                (addr, false)
            }
            Zpg => {
                self.pc = self.pc.wrapping_add(1);
                (self.data as u16, false)
            }
            AbsX => {
                self.pc = self.pc.wrapping_add(1);
                self.ptr = self.x;
                let lo = self.lo.wrapping_add(self.ptr);
                let addr = ((self.data as u16) << 8) | (lo as u16);
                (addr, false)
            }
            AbsY => {
                self.pc = self.pc.wrapping_add(1);
                self.ptr = self.y;
                let lo = self.lo.wrapping_add(self.ptr);
                let addr = ((self.data as u16) << 8) | (lo as u16);
                (addr, false)
            }
            AbsIx => {
                if self.lo.overflowing_add(self.ptr).1 {
                    self.addr = self.addr.wrapping_add(0x100);
                }
                (self.addr, false)
            }
            ZpgX => {
                let addr = (self.addr as u8).wrapping_add(self.x) as u16;
                (addr, false)
            }
            ZpgY => {
                let addr = (self.addr as u8).wrapping_add(self.y) as u16;
                (addr, false)
            }
            OraIx => {
                if self.lo.overflowing_add(self.ptr).1 {
                    let addr = self.addr.wrapping_add(0x100);
                    (addr, false)
                } else {
                    self.set_a_nz(self.a | self.data);
                    self.insn_cycle = 0;
                    (self.pc, false)
                }
            }
            AndIx => {
                if self.lo.overflowing_add(self.ptr).1 {
                    let addr = self.addr.wrapping_add(0x100);
                    (addr, false)
                } else {
                    self.set_a_nz(self.a & self.data);
                    self.insn_cycle = 0;
                    (self.pc, false)
                }
            }
            EorIx => {
                if self.lo.overflowing_add(self.ptr).1 {
                    let addr = self.addr.wrapping_add(0x100);
                    (addr, false)
                } else {
                    self.set_a_nz(self.a ^ self.data);
                    self.insn_cycle = 0;
                    (self.pc, false)
                }
            }
            LdaIx => {
                if self.lo.overflowing_add(self.ptr).1 {
                    let addr = self.addr.wrapping_add(0x100);
                    (addr, false)
                } else {
                    self.set_a_nz(self.data);
                    self.insn_cycle = 0;
                    (self.pc, false)
                }
            }
            LdxIx => {
                if self.lo.overflowing_add(self.ptr).1 {
                    let addr = self.addr.wrapping_add(0x100);
                    (addr, false)
                } else {
                    self.set_x_nz(self.data);
                    self.insn_cycle = 0;
                    (self.pc, false)
                }
            }
            LdyIx => {
                if self.lo.overflowing_add(self.ptr).1 {
                    let addr = self.addr.wrapping_add(0x100);
                    (addr, false)
                } else {
                    self.set_y_nz(self.data);
                    self.insn_cycle = 0;
                    (self.pc, false)
                }
            }
            AdcIx => {
                if self.lo.overflowing_add(self.ptr).1 {
                    let addr = self.addr.wrapping_add(0x100);
                    (addr, false)
                } else {
                    self.adc(self.data);
                    self.insn_cycle = 0;
                    (self.pc, false)
                }
            }
            CmpIx => {
                if self.lo.overflowing_add(self.ptr).1 {
                    let addr = self.addr.wrapping_add(0x100);
                    (addr, false)
                } else {
                    self.cmp(self.a, self.data);
                    self.insn_cycle = 0;
                    (self.pc, false)
                }
            }
            SbcIx => {
                if self.lo.overflowing_add(self.ptr).1 {
                    let addr = self.addr.wrapping_add(0x100);
                    (addr, false)
                } else {
                    self.sbc(self.data);
                    self.insn_cycle = 0;
                    (self.pc, false)
                }
            }
            IndY2 => {
                self.lo = self.data;
                let addr = (self.addr as u8).wrapping_add(1) as u16;
                (addr, false)
            }
            IndY3 => {
                let lo = self.lo.wrapping_add(self.y);
                self.ptr = self.y;
                let addr = ((self.data as u16) << 8) | (lo as u16);
                (addr, false)
            }
            XInd2 => {
                let addr = (self.addr as u8).wrapping_add(self.x) as u16;
                (addr, false)
            }
            XInd4 => {
                let addr = ((self.data as u16) << 8) | (self.lo as u16);
                (addr, false)
            }
            StaAbsIx => {
                if self.lo.overflowing_add(self.ptr).1 {
                    self.addr = self.addr.wrapping_add(0x100);
                }
                self.data = self.a;
                (self.addr, true)
            }
            Ind => {
                self.lo = self.data;
                // increment lo byte only; no carry
                let lo = (self.addr as u8).wrapping_add(1);
                let addr = (self.addr & 0xff00) | lo as u16;
                (addr, false)
            }
            StaZpg => {
                self.pc = self.pc.wrapping_add(1);
                let addr = self.data as u16;
                self.data = self.a;
                (addr, true)
            }
            StxZpg => {
                self.pc = self.pc.wrapping_add(1);
                let addr = self.data as u16;
                self.data = self.x;
                (addr, true)
            }
            StyZpg => {
                self.pc = self.pc.wrapping_add(1);
                let addr = self.data as u16;
                self.data = self.y;
                (addr, true)
            }
            StaZpgX => {
                self.data = self.a;
                let addr = (self.addr as u8).wrapping_add(self.x);
                (addr as u16, true)
            }
            StxZpgY => {
                self.data = self.x;
                let addr = (self.addr as u8).wrapping_add(self.y);
                (addr as u16, true)
            }
            StyZpgX => {
                self.data = self.y;
                let addr = (self.addr as u8).wrapping_add(self.x);
                (addr as u16, true)
            }
            StaAbs => {
                self.pc = self.pc.wrapping_add(1);
                let addr = ((self.data as u16) << 8) | (self.lo as u16);
                self.data = self.a;
                (addr, true)
            }
            StxAbs => {
                self.pc = self.pc.wrapping_add(1);
                let addr = ((self.data as u16) << 8) | (self.lo as u16);
                self.data = self.x;
                (addr, true)
            }
            StyAbs => {
                self.pc = self.pc.wrapping_add(1);
                let addr = ((self.data as u16) << 8) | (self.lo as u16);
                self.data = self.y;
                (addr, true)
            }
            StaXInd => {
                let addr = ((self.data as u16) << 8) | (self.lo as u16);
                self.data = self.a;
                (addr, true)
            }
            Txs => {
                self.sp = self.x;
                self.insn_cycle = 0;
                (self.pc, false)
            }
            Tsx => {
                self.set_x_nz(self.sp);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            Tax => {
                self.set_x_nz(self.a);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            Tay => {
                self.set_y_nz(self.a);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            Txa => {
                self.set_a_nz(self.x);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            Tya => {
                self.set_a_nz(self.y);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            Jmp => {
                self.pc = ((self.data as u16) << 8) | (self.lo as u16);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            Bpl => self.cond_branch((self.flags & 0x80) == 0),
            Bmi => self.cond_branch((self.flags & 0x80) != 0),
            Bvc => self.cond_branch((self.flags & 0x40) == 0),
            Bvs => self.cond_branch((self.flags & 0x40) != 0),
            Bcc => self.cond_branch((self.flags & 0x01) == 0),
            Bcs => self.cond_branch((self.flags & 0x01) != 0),
            Bne => self.cond_branch((self.flags & 0x02) == 0),
            Beq => self.cond_branch((self.flags & 0x02) != 0),
            CondBr => {
                let new_pc = self.pc.wrapping_add(self.lo as i8 as u16);
                let nocarry_pc = (self.pc & 0xff00) | (new_pc & 0xff);
                self.pc = new_pc;
                if new_pc == nocarry_pc {
                    self.insn_cycle = 0;
                }
                (nocarry_pc, false)
            }
            Dex => {
                let x = self.x.wrapping_sub(1);
                self.set_x_nz(x);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            Dey => {
                let y = self.y.wrapping_sub(1);
                self.set_y_nz(y);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            Inx => {
                let x = self.x.wrapping_add(1);
                self.set_x_nz(x);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            Iny => {
                let y = self.y.wrapping_add(1);
                self.set_y_nz(y);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            Pha => {
                self.data = self.a;
                let addr = 0x100 | (self.sp as u16);
                self.sp = self.sp.wrapping_sub(1);
                (addr, true)
            }
            Php => {
                self.data = self.flags;
                let addr = 0x100 | (self.sp as u16);
                self.sp = self.sp.wrapping_sub(1);
                (addr, true)
            }
            Pull1 => {
                let addr = 0x100 | (self.sp as u16);
                self.sp = self.sp.wrapping_add(1);
                (addr, false)
            }
            Pull2 => {
                let addr = 0x100 | (self.sp as u16);
                (addr, false)
            }
            Plp => {
                self.flags = self.data | 0x30;
                self.insn_cycle = 0;
                (self.pc, false)
            }
            Jsr1 => {
                self.pc = self.pc.wrapping_add(1);
                self.lo = self.data;
                let addr = 0x100 | (self.sp as u16);
                (addr, false)
            }
            Jsr2 => {
                self.data = (self.pc >> 8) as u8;
                self.sp = self.sp.wrapping_sub(1);
                (self.addr, true)
            }
            Jsr3 => {
                self.data = self.pc as u8;
                let addr = 0x100 | (self.sp as u16);
                self.sp = self.sp.wrapping_sub(1);
                (addr, true)
            }
            Jsr4 => (self.pc, false),
            Rts3 => {
                self.lo = self.data;
                let addr = 0x100 | (self.sp as u16);
                (addr, false)
            }
            Rts4 => {
                self.pc = ((self.data as u16) << 8) | (self.lo as u16);
                (self.pc, false)
            }
            Rts5 => {
                self.pc = self.pc.wrapping_add(1);
                self.insn_cycle = 0;
                (self.pc, false)
            }
            Rti3 => {
                self.flags = self.data;
                let addr = 0x100 | (self.sp as u16);
                self.sp = self.sp.wrapping_add(1);
                (addr, false)
            }
            Brk1 => {
                self.pc = self.pc.wrapping_add(1);
                self.data = (self.pc >> 8) as u8;
                let addr = 0x100 | (self.sp as u16);
                self.sp = self.sp.wrapping_sub(1);
                (addr, true)
            }
            Brk3 => {
                self.data = self.flags;
                let addr = 0x100 | (self.sp as u16);
                self.sp = self.sp.wrapping_sub(1);
                (addr, true)
            }
            Brk4 => (0xfffe, false),
            Brk5 => {
                self.lo = self.data;
                (0xffff, false)
            }
            Brk6 => {
                self.pc = ((self.data as u16) << 8) | (self.lo as u16);
                self.flags |= 0x04;
                self.insn_cycle = 0;
                (self.pc, false)
            }
            _ => panic!("not yet implemented"),
        };
        self.addr = addr;
        self.is_write = is_write
    }

    fn cond_branch(&mut self, cond: bool) -> (u16, bool) {
        self.pc = self.pc.wrapping_add(1);
        if cond {
            self.lo = self.data;
        } else {
            self.insn_cycle = 0;
        }
        (self.pc, false)
    }
}
