# R6502

This is my take on emulation of the venerable 6502 microprocessor. Goals include:

* High fidelity, cycle accurate emulation.
* Reasonably small code size.
* Reasonably good performance.
* Easy to use as a component, integrating with I/O.

It is likely this will grow into an Apple II emulator, very possibly running on a Raspberry Pi Pico, with bit-banged DVI video output (using [pico-dvi-rs]).

One non-goal is to implement finer grained micro-architectural emulation; it would be difficult to achieve high performance.

Internally, the implementation uses a form of microcode. There are up to seven micro-operations per instruction, each corresponding to one clock tick and thus one bus cycle. As in the original 6502, on each cycle there is either a read or write operation on the bus. The main method on the `Cpu` performs a clock tick. It is then up to the client to perform the bus operation, whether that's reading or writing to a simulated RAM or doing IO.

It currently passes the [6502 functional tests]. A goal is to make it also pass the [Tom Harte test suite], which will require careful implementation of illegal instructions, and likely some other refinements.

The current status is rough and unfinished. The core is probably reasonably stable, other than making it pass a more rigorous test suite and making it log less (which in turn precludes no_std), but things will move around to become better organized, and possibly to support emulation of more of the Apple II than just the CPU.

To try it, run:

```
cargo run --release 6502_functional_test.bin > log.txt
```

This generates several gigabytes of log files.

It is licensed under your choice of Apache 2 or MIT.

[6502 functional tests]: https://github.com/Klaus2m5/6502_65C02_functional_tests
[pico-dvi-rs]: https://github.com/DusterTheFirst/pico-dvi-rs
[Tom Harte test suite]: https://github.com/TomHarte/ProcessorTests
