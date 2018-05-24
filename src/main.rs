use std::env;
use std::fs::File;
use std::io::Read;

mod cpu;
use cpu::{Bus, Cpu};

struct DebugBus;

impl Bus for DebugBus {
    fn read(&mut self, addr: u16, cpu: &mut Cpu) -> u8 {
        let val = cpu.mem[addr as usize];
        println!("rd {:04x} {:02x}", addr, val);
        val
    }

    fn write(&mut self, addr: u16, val: u8, cpu: &mut Cpu) {
        println!("wr {:04x} {:02x}", addr, val);
        cpu.mem[addr as usize] = val;
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
    cpu.mem.copy_from_slice(&bin);
    cpu.pc = 0x400;
    cpu.set_masks(!0, !0);
    let mut debug_bus = DebugBus;
    cpu.step(&mut debug_bus);
}
