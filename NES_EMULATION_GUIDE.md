# NES Emulation Guide

This guide explains how NES emulation has been added to the Ochip emulator.

## Overview

The Ochip emulator now supports both CHIP-8 and NES (Nintendo Entertainment System) emulation. The system automatically detects which emulator to use based on the ROM file extension.

## Architecture

### NES Core Components

The NES emulation is implemented in `NES.vb` with three main classes:

#### 1. CPU6502
- Implements the 6502 microprocessor used in the NES
- Handles instruction execution and register management
- Current implementation includes basic opcodes (LDA, STA, JMP, JSR, RTS, NOP)
- Executes approximately 1.79 MHz worth of cycles per frame

#### 2. PPU (Picture Processing Unit)
- Manages graphics rendering
- Runs at 3x the speed of the CPU (~5.37 MHz)
- Renders 256x240 display at 60 Hz
- Current implementation provides a basic frame rendering structure

#### 3. Memory
- Manages memory mapping for the NES
- Handles 2KB internal RAM (mirrored)
- Loads and manages cartridge ROM data (PRG-ROM and CHR-ROM)
- Parses iNES format ROM files
- Supports basic mapper functionality

### UI Integration

The main form (`Form1.vb`) has been updated to:
- Support both CHIP-8 and NES emulation modes
- Automatically detect ROM type by file extension
- Resize window appropriately for each system (64x32 for CHIP-8, 256x240 for NES)
- Map keyboard inputs to NES controller buttons
- Render display buffers for both systems

## Using NES Emulation

### Loading a NES ROM

1. Click "Choose ROM..." button
2. Select a `.nes` file (iNES format)
3. The emulator will automatically switch to NES mode
4. The window will resize to 256x240 pixels
5. The ROM will start running

### Controller Mapping

```
Keyboard Key    NES Button
Z               A
X               B
Backspace       Select
Enter           Start
Arrow Keys      D-pad
```

### Supported ROM Format

The emulator supports iNES format ROM files (.nes):
- Header: 16 bytes containing ROM metadata
- PRG-ROM: Program code (in 16KB units)
- CHR-ROM: Character/graphics data (in 8KB units)
- Mapper information extracted from header

## Current Limitations

The NES emulation is a basic implementation with the following limitations:

1. **CPU**: Only a subset of 6502 instructions are implemented
2. **PPU**: Basic structure only - no actual pattern table, sprite, or background rendering
3. **APU**: Not implemented (no sound)
4. **Mappers**: Only basic support for Mapper 0
5. **Accuracy**: Timing is approximate, not cycle-accurate

## Future Enhancements

To improve NES emulation, the following could be added:

1. Complete 6502 instruction set implementation
2. Full PPU implementation with:
   - Pattern table rendering
   - Nametable and attribute table handling
   - Sprite rendering with priority
   - Scrolling support
3. APU implementation for sound
4. Additional mapper support (Mapper 1, 2, 3, etc.)
5. Save state support
6. Controller 2 support

## Testing

To test the NES emulation:

1. Build the project using Visual Studio or `dotnet build`
2. Run the executable
3. Load a simple NES ROM (homebrew ROMs work well for testing)
4. Verify the window resizes correctly
5. Test controller inputs

## Technical References

- [NESDev Wiki](https://wiki.nesdev.com/) - Comprehensive NES technical documentation
- [6502 Instruction Set](http://www.6502.org/tutorials/6502opcodes.html) - Complete opcode reference
- [iNES Format](https://wiki.nesdev.com/w/index.php/INES) - ROM file format specification
