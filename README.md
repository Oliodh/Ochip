# Ochip - Multi-System Emulator

A multi-system emulator written in Visual Basic .NET using Windows Forms. This emulator provides accurate implementations of classic gaming systems, starting with CHIP-8 and NES (Nintendo Entertainment System).

## Supported Systems

### CHIP-8
CHIP-8 is an interpreted programming language developed in the mid-1970s. It was initially used on the COSMAC VIP and Telmac 1800 8-bit microcomputers to make game programming easier. CHIP-8 programs run on a CHIP-8 virtual machine with:
- 4KB of memory
- 16 general-purpose 8-bit registers (V0-VF)
- 64x32 monochrome display (or 128x64 in Super-CHIP mode)
- 16-key hexadecimal keypad
- Simple sound timer

### Super-CHIP (SCHIP)
The emulator also supports Super-CHIP extensions, which were developed for the HP48 calculators in the early 1990s. Super-CHIP adds:
- High-resolution mode (128x64 pixels)
- 16x16 sprite support
- Scrolling instructions (scroll down, left, and right)
- Extended 10-byte font set for high-resolution mode
- RPL user flags for persistent storage
- Backwards compatibility with standard CHIP-8 ROMs

### NES (Nintendo Entertainment System)
The NES is an 8-bit home video game console released by Nintendo in 1983. The emulator supports:
- Full 6502 CPU emulation with complete instruction set
- PPU (Picture Processing Unit) for graphics rendering
- VBlank and NMI interrupt support
- Controller input handling
- 256x240 display resolution
- Controller input (8 buttons)
- iNES ROM format (.nes files)

## Features

- **Multi-system emulation** - Supports CHIP-8, Super-CHIP, and NES systems
- **Automatic ROM detection** - Automatically detects system type based on file extension
- **CHIP-8 Features**:
  - Full CHIP-8 instruction set implementation
  - Super-CHIP (SCHIP) extension support
  - Dynamic display resolution (64x32 or 128x64)
  - ROM loading (.ch8, .c8, .rom files)
  - Accurate 60 Hz timing
  - Full keyboard input support
  - Display scrolling (Super-CHIP)
  - Extended sprites and fonts (Super-CHIP)
- **NES Features**:
  - Full 6502 CPU emulation with complete instruction set
  - PPU (Picture Processing Unit) for graphics
  - VBlank and NMI interrupt handling
  - 256x240 display resolution
  - iNES ROM format support (.nes files)
  - Controller input mapping
  - Background rendering with pattern tables and nametables
- **Emulation controls**:
  - Load ROM file via file picker
  - Reset emulation
  - Pause/Resume execution

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
2. Click the "Choose ROM..." button to select a ROM file
3. The emulator will automatically detect the system type:
   - `.ch8`, `.c8`, `.rom` files → CHIP-8 emulator
   - `.nes` files → NES emulator
4. The ROM will load and start running automatically
5. Use the control buttons:
   - **Reset**: Restart the currently loaded ROM
   - **Pause/Resume**: Pause or resume emulation

## Usage

### Loading ROMs

Click the "Choose ROM..." button and select a ROM file. The emulator supports:

**CHIP-8 ROMs:**
- `.ch8`
- `.c8`
- `.rom`

**NES ROMs:**
- `.nes` (iNES format)

### Keyboard Controls

#### CHIP-8 Keyboard Mapping

The emulator maps PC keyboard keys to CHIP-8's hexadecimal keypad:

**Standard Mapping:**
```
PC Keyboard    CHIP-8 Key
1 2 3 4    →   1 2 3 C
Q W E R    →   4 5 6 D
A S D F    →   7 8 9 E
Z X C V    →   A 0 B F
```

**Arrow Keys (for convenience):**
- **Up Arrow**: CHIP-8 key C
- **Down Arrow**: CHIP-8 key D
- **Left Arrow**: CHIP-8 key 1
- **Right Arrow**: CHIP-8 key 4

#### NES Controller Mapping

The emulator maps PC keyboard keys to NES controller buttons:

- **Z**: A button
- **X**: B button
- **Backspace**: Select
- **Enter**: Start
- **Arrow Keys**: D-pad (Up/Down/Left/Right)

## Technical Details

### CHIP-8 Specifications

- **Display Resolution**: 64x32 pixels (low-res mode) or 128x64 pixels (high-res Super-CHIP mode)
- **Display Scale**: 10x for low-res (640x320 window), 5x for high-res (640x320 window)
- **CPU Cycles**: 12 cycles per frame
- **Frame Rate**: 60 Hz (~16.67ms per frame)
- **Memory**: 4096 bytes (4KB)
- **Program Counter Start**: 0x200
- **Registers**: 16 general-purpose 8-bit registers (V0-VF)
- **Stack**: 16 levels
- **Timers**: Delay timer and sound timer (both decrement at 60 Hz)

### Super-CHIP Extensions

- **High-Resolution Mode**: 128x64 pixels (enabled with 00FF opcode)
- **Low-Resolution Mode**: 64x32 pixels (enabled with 00FE opcode)
- **Scrolling**: Scroll down N pixels (00CN), scroll left 4 pixels (00FC), scroll right 4 pixels (00FB)
- **Extended Sprites**: 16x16 pixel sprites in high-res mode (DXY0 when N=0)
- **Extended Font**: 10-byte 8x10 font set for high-resolution mode (FX30)
- **RPL User Flags**: 16 bytes of persistent storage (FX75 to store, FX85 to load)
- **Exit Instruction**: 00FD to exit the interpreter

### NES Specifications

- **CPU**: 6502 processor running at ~1.79 MHz
  - Complete instruction set implementation
  - All addressing modes supported
  - NMI interrupt support
- **PPU**: Picture Processing Unit running at ~5.37 MHz
  - Background rendering
  - VBlank flag and interrupt
- **Display Resolution**: 256x240 pixels
- **Frame Rate**: ~60 Hz
- **Memory**: 2KB internal RAM
- **ROM Format**: iNES format (.nes files)
- **Mapper Support**: Basic mapper support (currently Mapper 0)

### Architecture

The emulator consists of multiple components:

#### CHIP-8 Components

1. **Chip8.vb** - Core CHIP-8 virtual machine implementation
   - Memory management
   - CPU instruction execution (CHIP-8 + Super-CHIP extensions)
   - Display and input handling (supports both 64x32 and 128x64 resolutions)
   - Timer management
   - Scrolling operations
   - Extended fonts and sprites

#### NES Components

1. **NES.vb** - Core NES emulation
   - **CPU6502** - Complete 6502 processor emulation with all instructions
   - **PPU** - Picture Processing Unit for graphics with VBlank/NMI support
   - **Memory** - Memory management, cartridge loading, and controller I/O
   
#### UI Components

2. **Form1.vb** - Windows Forms GUI
   - ROM file loading with automatic system detection
   - Display rendering for both CHIP-8 and NES
   - Dynamic window resizing for CHIP-8 resolution changes
   - Input handling and keyboard mapping for both systems
   - Emulation control (pause, reset)

## License

This project is licensed under the MIT License. See the [LICENSE.txt](LICENSE.txt) file for details.

## Resources

### CHIP-8 Resources
- [CHIP-8 Wikipedia](https://en.wikipedia.org/wiki/CHIP-8)
- [Cowgod's CHIP-8 Technical Reference](http://devernay.free.fr/hacks/chip8/C8TECH10.HTM)
- [Super-CHIP Documentation](http://devernay.free.fr/hacks/chip8/schip.txt)
- [CHIP-8 Extensions Reference](https://github.com/chip-8/extensions)
- [CHIP-8 ROMs Archive](https://github.com/kripod/chip8-roms)

### NES Resources
- [NES Wikipedia](https://en.wikipedia.org/wiki/Nintendo_Entertainment_System)
- [NESDev Wiki](https://wiki.nesdev.com/)
- [6502 CPU Reference](http://www.6502.org/)
- [NES ROM Archive](https://github.com/topics/nes-roms)