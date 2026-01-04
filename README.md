# Ochip - CHIP-8 Emulator

A CHIP-8 emulator written in Visual Basic .NET using Windows Forms. This emulator provides an accurate implementation of the classic CHIP-8 virtual machine, allowing you to run classic CHIP-8 games and programs.

## About CHIP-8

CHIP-8 is an interpreted programming language developed in the mid-1970s. It was initially used on the COSMAC VIP and Telmac 1800 8-bit microcomputers to make game programming easier. CHIP-8 programs run on a CHIP-8 virtual machine with:
- 4KB of memory
- 16 general-purpose 8-bit registers (V0-VF)
- 64x32 monochrome display
- 16-key hexadecimal keypad
- Simple sound timer

## Features

- **Full CHIP-8 instruction set implementation** - Supports all standard CHIP-8 opcodes
- **64x32 pixel display** - Monochrome graphics with configurable scaling
- **ROM loading** - Load and run CHIP-8 ROM files (.ch8, .c8, .rom)
- **Emulation controls**:
  - Load ROM file via file picker
  - Reset emulation
  - Pause/Resume execution
- **Accurate timing** - 60 Hz frame rate with configurable cycles per frame
- **Keyboard input support** - Maps keyboard keys to CHIP-8 hexadecimal keypad

## Requirements

- **Operating System**: Windows (uses Windows Forms)
- **.NET**: .NET 10.0 or later
- **IDE** (for building): Visual Studio 2022 or later with VB.NET support

## Building

1. Clone the repository:
   ```bash
   git clone https://github.com/Oliodh/Ochip.git
   cd Ochip
   ```

2. Open the solution:
   - Open `WinFormsApp16.slnx` in Visual Studio 2022 or later

3. Build the project:
   - Press `Ctrl+Shift+B` or select **Build > Build Solution** from the menu

## Running

1. Run the application from Visual Studio (F5) or execute the built executable
2. Click the "Choose ROM..." button to select a CHIP-8 ROM file
3. The emulator will load and start running the ROM automatically
4. Use the control buttons:
   - **Reset**: Restart the currently loaded ROM
   - **Pause/Resume**: Pause or resume emulation

## Usage

### Loading ROMs

Click the "Choose ROM..." button and select a CHIP-8 ROM file. The emulator supports the following file extensions:
- `.ch8`
- `.c8`
- `.rom`
- Any file (*.*)

### Keyboard Controls

The emulator currently maps some keyboard keys to CHIP-8's hexadecimal keypad:
- **Up Arrow**: CHIP-8 key C
- **Down Arrow**: CHIP-8 key D

(Additional key mappings can be found in the source code)

## Technical Details

### Emulator Specifications

- **Display Resolution**: 64x32 pixels
- **Display Scale**: 10x (640x320 window)
- **CPU Cycles**: 12 cycles per frame
- **Frame Rate**: 60 Hz (~16.67ms per frame)
- **Memory**: 4096 bytes (4KB)
- **Program Counter Start**: 0x200
- **Registers**: 16 general-purpose 8-bit registers (V0-VF)
- **Stack**: 16 levels
- **Timers**: Delay timer and sound timer (both decrement at 60 Hz)

### Architecture

The emulator consists of two main components:

1. **Chip8.vb** - Core CHIP-8 virtual machine implementation
   - Memory management
   - CPU instruction execution
   - Display and input handling
   - Timer management

2. **Form1.vb** - Windows Forms GUI
   - ROM file loading
   - Display rendering with nearest-neighbor scaling
   - Input handling and keyboard mapping
   - Emulation control (pause, reset)

## License

This project is licensed under the MIT License. See the [LICENSE.txt](LICENSE.txt) file for details.

## Resources

- [CHIP-8 Wikipedia](https://en.wikipedia.org/wiki/CHIP-8)
- [Cowgod's CHIP-8 Technical Reference](http://devernay.free.fr/hacks/chip8/C8TECH10.HTM)
- [CHIP-8 ROMs Archive](https://github.com/kripod/chip8-roms)