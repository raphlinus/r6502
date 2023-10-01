use std::env;
use std::fs::File;
use std::io::Read;

mod cpu;
use cpu::{Bus, Cpu};

struct DebugBus {
    mem: [u8; 65536],
}

impl Bus for DebugBus {
    fn read(&mut self, addr: u16) -> u8 {
        let val = self.mem[addr as usize];
        println!("rd {:04x} {:02x}", addr, val);
        val
    }

    fn write(&mut self, addr: u16, val: u8) {
        println!("wr {:04x} {:02x}", addr, val);
        self.mem[addr as usize] = val;
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("need bin arg");
        return;
    }
    let mut f = File::open(&args[1]).expect("file not found");
    let mut bin = Vec::new();
    f.read_to_end(&mut bin).expect("read error");
    println!("size = {}", bin.len());
    let mut cpu = Cpu::new();
    let mut debug_bus = DebugBus {
        mem: [0; 65536],
    };
    debug_bus.mem.copy_from_slice(&bin);
    cpu.pc = 0x400;
    cpu.addr = 0x400;
    let mut last_insn_addr = 0;
    loop {
        cpu.do_bus(&mut debug_bus);
        cpu.print_state();
        if cpu.insn_cycle == 1 {
            if cpu.addr == last_insn_addr {
                break;
            }
            last_insn_addr = cpu.addr;
        }
        cpu.cpu_clk_ucode();
    }
}
