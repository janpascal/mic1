# Microprogramming, a mic1/mac1 simulator written in Pascal

## Introduction

While searching for some old documents, I came across an old simulator for Tanenbaum's MIC-1 microarchitecture from his books Structured Computer Organisation. I wrote the simulator while in University in 1991, in Turbo Pascal with the Turbo Vision text-mode UI library. It took amazingly little to get the thing working using the Free Pascal compiler and libraries. Most of the work was in converting 8086 assembler code to Pascal. I think I had originally used assembler because of performance reasons, or maybe the whole project started as an assembly project and I later added the UI using Pascal and Turbo Vision.

## Features

- MIC-1 microcode assembler (micasm)
- MAC-1 assembler (macasm)
- Full simulator (micropro)
- Shows microcode memory, main memory, all registers, latches and other internals (ALU, MMUX, LatchA, LatchB, MAR, MBR, IR, MIR, MPC)
- Single-step on clock phase (1-4), micro instruction or macro instruction, "run to" feature
- Simulates character output. No input.
- Includes microcode for the MAC-1 instruction set (micro.mic)
- Includes an example MAC-1 program that fills two vectors with numbers, calculates the vector product, and output the result in hex

## Building from source code

Install the needed compiler and libraries. If, like me, you're using Debian, use (as root):

```
# apt-get install fpc make
```

Compile the simulator and tools and assemble the microcode and demonstration assembler program by running
```
$ make
```
Finally,  run the simulator:
```
$ make run
```

## License

Copyright (C) 1991,2011, 2023 Jan-Pascal van Best <janpascal@vanbest.org>
   
MicroProgramming is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

MicroProgramming is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with MicroProgramming.  If not, see <http://www.gnu.org/licenses/>

## Screen shot

![afbeelding](https://github.com/janpascal/mic1/assets/1530584/b86224a7-a401-4616-95bb-6a70aa301cda)
