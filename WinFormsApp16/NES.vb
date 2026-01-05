Option Strict On
Option Explicit On

Imports System.IO

Public NotInheritable Class NES
    ' NES Display specifications
    Public Const DisplayWidth As Integer = 256
    Public Const DisplayHeight As Integer = 240

    ' CPU 6502
    Private _cpu As CPU6502
    Private _ppu As PPU
    Private _memory As Memory

    ' Display buffer (RGB format for Windows Forms)
    Public ReadOnly DisplayBuffer(DisplayWidth * DisplayHeight - 1) As Integer

    ' Controller state
    Public ReadOnly Controller1(7) As Boolean ' 8 buttons: A, B, Select, Start, Up, Down, Left, Right
    Public ReadOnly Controller2(7) As Boolean

    Public DrawFlag As Boolean

    Public Sub New()
        _memory = New Memory()
        _cpu = New CPU6502(_memory)
        _ppu = New PPU(_memory, DisplayBuffer)
        _memory.SetPPU(_ppu)
        Reset()
    End Sub

    Public Sub Reset()
        _cpu.Reset()
        _ppu.Reset()
        Array.Clear(Controller1, 0, Controller1.Length)
        Array.Clear(Controller2, 0, Controller2.Length)
        DrawFlag = True
    End Sub

    Public Sub LoadRom(path As String)
        Dim romData As Byte() = File.ReadAllBytes(path)
        _memory.LoadCartridge(romData)
        _cpu.Reset()
        _ppu.Reset()
        DrawFlag = True
    End Sub

    Public Sub SetController1Button(button As Integer, isDown As Boolean)
        If button >= 0 AndAlso button < 8 Then
            Controller1(button) = isDown
        End If
    End Sub

    Public Sub ExecuteFrame()
        ' NES runs at ~60 Hz, execute one frame worth of cycles
        ' CPU runs at ~1.79 MHz, PPU at ~5.37 MHz
        ' One frame = 29780.5 CPU cycles (calculated as: 262 scanlines * 113.667 cycles/scanline)
        Const CyclesPerFrame As Integer = 29781

        Dim cycles As Integer = 0
        While cycles < CyclesPerFrame
            Dim cpuCycles As Integer = _cpu.ExecuteInstruction()
            cycles += cpuCycles

            ' PPU runs 3 times faster than CPU
            For i As Integer = 0 To (cpuCycles * 3) - 1
                _ppu.ExecuteCycle()
                If _ppu.FrameComplete Then
                    DrawFlag = True
                    _ppu.FrameComplete = False
                End If
            Next
        End While
    End Sub

    ' Inner classes for NES components

    Public NotInheritable Class CPU6502
        Private ReadOnly _memory As Memory

        ' 6502 Registers
        Public A As Byte        ' Accumulator
        Public X As Byte        ' X register
        Public Y As Byte        ' Y register
        Public PC As UShort     ' Program Counter
        Public SP As Byte       ' Stack Pointer
        Public P As Byte        ' Processor Status

        ' Status flags
        Private Const FlagC As Byte = &H1    ' Carry
        Private Const FlagZ As Byte = &H2    ' Zero
        Private Const FlagI As Byte = &H4    ' Interrupt Disable
        Private Const FlagD As Byte = &H8    ' Decimal Mode (not used in NES)
        Private Const FlagB As Byte = &H10   ' Break
        Private Const FlagU As Byte = &H20   ' Unused (always 1)
        Private Const FlagV As Byte = &H40   ' Overflow
        Private Const FlagN As Byte = &H80   ' Negative

        Public Sub New(memory As Memory)
            _memory = memory
            Reset()
        End Sub

        Public Sub Reset()
            A = 0
            X = 0
            Y = 0
            SP = &HFD
            P = FlagU Or FlagI
            ' Reset vector at $FFFC-$FFFD
            PC = CUShort(_memory.Read(&HFFFC) Or (CUShort(_memory.Read(&HFFFD)) << 8))
        End Sub

        Public Function ExecuteInstruction() As Integer
            ' Simplified 6502 instruction execution
            ' Returns number of cycles taken
            Dim opcode As Byte = _memory.Read(PC)
            PC = CUShort(PC + 1US)

            ' Basic instruction set (simplified for demonstration)
            Select Case opcode
                Case &HEA ' NOP
                    Return 2

                Case &HA9 ' LDA Immediate
                    A = _memory.Read(PC)
                    PC = CUShort(PC + 1US)
                    SetZN(A)
                    Return 2

                Case &HAD ' LDA Absolute
                    Dim addr As UShort = ReadWord(PC)
                    PC = CUShort(PC + 2US)
                    A = _memory.Read(addr)
                    SetZN(A)
                    Return 4

                Case &H85 ' STA Zero Page
                    Dim zpAddr As Byte = _memory.Read(PC)
                    PC = CUShort(PC + 1US)
                    _memory.Write(zpAddr, A)
                    Return 3

                Case &H8D ' STA Absolute
                    Dim absAddr As UShort = ReadWord(PC)
                    PC = CUShort(PC + 2US)
                    _memory.Write(absAddr, A)
                    Return 4

                Case &H4C ' JMP Absolute
                    PC = ReadWord(PC)
                    Return 3

                Case &H20 ' JSR
                    Dim target As UShort = ReadWord(PC)
                    PC = CUShort(PC + 2US)  ' JSR has 2-byte operand
                    ' 6502 pushes PC-1 to stack (return address - 1)
                    Dim returnAddr As UShort = CUShort(PC - 1US)
                    ' Push high byte first, then low byte (stack grows downward)
                    Push(CByte(returnAddr >> 8))
                    Push(CByte(returnAddr And &HFF))
                    PC = target
                    Return 6

                Case &H60 ' RTS
                    ' Stack grows downward: last pushed (low) is popped first
                    ' Pop() increments SP then reads, so this gets low byte first, then high byte
                    Dim lo As Byte = Pop()
                    Dim hi As Byte = Pop()
                    PC = CUShort((CUShort(hi) << 8) Or lo)
                    ' Add 1 to the return address (6502 convention)
                    PC = CUShort(PC + 1US)
                    Return 6

                Case Else
                    ' Unimplemented instruction - just NOP
                    Return 2
            End Select
        End Function

        Private Function ReadWord(address As UShort) As UShort
            Dim lo As Byte = _memory.Read(address)
            Dim hi As Byte = _memory.Read(CUShort(address + 1US))
            Return CUShort((CUShort(hi) << 8) Or lo)
        End Function

        Private Sub SetZN(value As Byte)
            If value = 0 Then
                P = CByte(P Or FlagZ)
            Else
                P = CByte(P And (Not FlagZ))
            End If

            If (value And &H80) <> 0 Then
                P = CByte(P Or FlagN)
            Else
                P = CByte(P And (Not FlagN))
            End If
        End Sub

        Private Sub Push(value As Byte)
            _memory.Write(CUShort(&H100US + SP), value)
            SP = CByte((SP - 1) And &HFF)
        End Sub

        Private Function Pop() As Byte
            SP = CByte((SP + 1) And &HFF)
            Return _memory.Read(CUShort(&H100US + SP))
        End Function
    End Class

    Public NotInheritable Class PPU
        Private ReadOnly _memory As Memory
        Private ReadOnly _displayBuffer As Integer()

        Private _cycle As Integer
        Private _scanline As Integer

        Public FrameComplete As Boolean

        ' PPU registers
        Private _ppuCtrl As Byte        ' $2000 PPUCTRL
        Private _ppuMask As Byte        ' $2001 PPUMASK
        Private _ppuStatus As Byte      ' $2002 PPUSTATUS
        Private _oamAddr As Byte        ' $2003 OAMADDR
        Private _ppuScroll As Byte      ' $2005 PPUSCROLL
        Private _ppuAddr As UShort      ' $2006 PPUADDR
        Private _ppuData As Byte        ' $2007 PPUDATA
        
        ' PPU internal state
        Private _addressLatch As Boolean
        Private _dataBuffer As Byte
        
        ' VRAM - 2KB for nametables (mirrored)
        Private ReadOnly _vram(2047) As Byte
        
        ' Palette RAM - 32 bytes
        Private ReadOnly _paletteRam(31) As Byte
        
        ' NES color palette (64 colors)
        Private Shared ReadOnly NesPalette As Integer() = {
            &HFF666666, &HFF002A88, &HFF1412A7, &HFF3B00A4, &HFF5C007E, &HFF6E0040, &HFF6C0600, &HFF561D00,
            &HFF333500, &HFF0B4800, &HFF005200, &HFF004F08, &HFF00404D, &HFF000000, &HFF000000, &HFF000000,
            &HFFADADAD, &HFF155FD9, &HFF4240FF, &HFF7527FE, &HFFA01ACC, &HFFB71E7B, &HFFB53120, &HFF994E00,
            &HFF6B6D00, &HFF388700, &HFF0C9300, &HFF008F32, &HFF007C8D, &HFF000000, &HFF000000, &HFF000000,
            &HFFFFFFE3, &HFF609DFF, &HFF9C84FF, &HFFC970FF, &HFFF364FF, &HFFFE6ECC, &HFFFE8170, &HFFEA9E22,
            &HFFBCBE00, &HFF88D800, &HFF5CE430, &HFF45E082, &HFF48CDDE, &HFF4F4F4F, &HFF000000, &HFF000000,
            &HFFFFFFFF, &HFFC0DFFF, &HFFD3D2FF, &HFFE8C8FF, &HFFFBC2FF, &HFFFEC4EA, &HFFFECCC5, &HFFF7D8A5,
            &HFFE4E594, &HFFCFEF96, &HFFBDF4AB, &HFFB3F3CC, &HFFB5EBF2, &HFFB8B8B8, &HFF000000, &HFF000000
        }

        Public Sub New(memory As Memory, displayBuffer As Integer())
            _memory = memory
            _displayBuffer = displayBuffer
            Reset()
        End Sub

        Public Sub Reset()
            _cycle = 0
            _scanline = 0
            FrameComplete = False
            _ppuCtrl = 0
            _ppuMask = 0
            _ppuStatus = &HA0  ' Bits 7 and 5 set (VBlank and sprite overflow flags cleared, unused bit set)
            _oamAddr = 0
            _ppuScroll = 0
            _ppuAddr = 0
            _ppuData = 0
            _addressLatch = False
            _dataBuffer = 0
            Array.Clear(_vram, 0, _vram.Length)
            Array.Clear(_paletteRam, 0, _paletteRam.Length)
            Array.Clear(_displayBuffer, 0, _displayBuffer.Length)
        End Sub

        Public Sub ExecuteCycle()
            ' Simplified PPU cycle execution
            _cycle += 1

            If _cycle >= 341 Then
                _cycle = 0
                _scanline += 1

                If _scanline >= 262 Then
                    _scanline = 0
                    FrameComplete = True
                    RenderFrame()
                End If
            End If
        End Sub

        Private Sub RenderFrame()
            ' Render the background using pattern tables and nametables
            ' This is a simplified implementation that renders one background layer
            
            ' Get base nametable address from PPUCTRL
            Dim nametableAddr As UShort = CUShort(&H2000US + ((_ppuCtrl And &H3) * &H400US))
            
            ' Get pattern table address for background (bit 4 of PPUCTRL)
            Dim patternTableAddr As UShort = If((_ppuCtrl And &H10) <> 0, &H1000US, &H0US)
            
            ' Render all 30 rows of 32 tiles each
            For tileY As Integer = 0 To 29
                For tileX As Integer = 0 To 31
                    ' Get tile index from nametable
                    Dim nametableOffset As UShort = CUShort(tileY * 32 + tileX)
                    Dim tileIndex As Byte = ReadVRAM(CUShort(nametableAddr + nametableOffset))
                    
                    ' Get attribute byte for this tile (determines palette)
                    ' Each attribute byte controls a 4x4 tile (32x32 pixel) region
                    ' Split into four 2x2 tile quadrants, each using 2 bits for palette selection
                    Dim attrX As Integer = tileX \ 4
                    Dim attrY As Integer = tileY \ 4
                    Dim attrOffset As UShort = CUShort(&H3C0US + (attrY * 8) + attrX)
                    Dim attrByte As Byte = ReadVRAM(CUShort(nametableAddr + attrOffset))
                    
                    ' Determine which 2 bits of the attribute byte to use based on tile position
                    ' Quadrant layout: top-left (bits 0-1), top-right (bits 2-3), 
                    '                  bottom-left (bits 4-5), bottom-right (bits 6-7)
                    Dim shift As Integer = ((tileX And 2) + ((tileY And 2) * 2))
                    Dim paletteNum As Byte = CByte((attrByte >> shift) And &H3)
                    
                    ' Read pattern data for this tile (8x8 pixels)
                    Dim tileAddr As UShort = CUShort(patternTableAddr + (tileIndex * 16US))
                    
                    For pixelY As Integer = 0 To 7
                        Dim planeLow As Byte = _memory.ReadCHR(CUShort(tileAddr + CUShort(pixelY)))
                        Dim planeHigh As Byte = _memory.ReadCHR(CUShort(tileAddr + CUShort(pixelY) + 8US))
                        
                        For pixelX As Integer = 0 To 7
                            ' Combine the two bit planes to get color index
                            Dim bit As Integer = 7 - pixelX
                            Dim colorLow As Byte = CByte((planeLow >> bit) And 1)
                            Dim colorHigh As Byte = CByte((planeHigh >> bit) And 1)
                            Dim colorIndex As Byte = CByte((colorHigh << 1) Or colorLow)
                            
                            ' Get color from palette
                            ' Color index 0 always uses the universal background color (palette address 0)
                            Dim paletteAddr As Integer = If(colorIndex = 0, 0, (paletteNum * 4) + colorIndex)
                            Dim paletteValue As Byte = _paletteRam(paletteAddr)
                            Dim color As Integer = NesPalette(paletteValue And &H3F)
                            
                            ' Draw pixel to display buffer
                            Dim screenX As Integer = (tileX * 8) + pixelX
                            Dim screenY As Integer = (tileY * 8) + pixelY
                            If screenX < DisplayWidth AndAlso screenY < DisplayHeight Then
                                Dim bufferIndex As Integer = (screenY * DisplayWidth) + screenX
                                _displayBuffer(bufferIndex) = color
                            End If
                        Next
                    Next
                Next
            Next
        End Sub
        
        Private Function ReadVRAM(address As UShort) As Byte
            ' VRAM address space $0000-$3FFF
            If address < &H2000US Then
                ' Pattern tables (CHR-ROM) - handled by Memory
                Return _memory.ReadCHR(address)
            ElseIf address < &H3F00US Then
                ' Nametables ($2000-$2FFF) - mirrored
                Dim offset As Integer = (address - &H2000US) And &H7FF
                Return _vram(offset)
            ElseIf address < &H4000US Then
                ' Palette RAM ($3F00-$3F1F) - mirrored
                Dim offset As Integer = (address - &H3F00US) And &H1F
                ' Mirror background color
                If (offset And &H3) = 0 Then
                    offset = 0
                End If
                Return _paletteRam(offset)
            Else
                Return 0
            End If
        End Function
        
        Public Sub WriteVRAM(address As UShort, value As Byte)
            If address < &H2000US Then
                ' Pattern tables (CHR-ROM) - read-only in most mappers
                ' Some mappers allow CHR-RAM, but we'll skip that for now
            ElseIf address < &H3F00US Then
                ' Nametables ($2000-$2FFF) - mirrored
                Dim offset As Integer = (address - &H2000US) And &H7FF
                _vram(offset) = value
            ElseIf address < &H4000US Then
                ' Palette RAM ($3F00-$3F1F) - mirrored
                Dim offset As Integer = (address - &H3F00US) And &H1F
                ' Mirror background color
                If (offset And &H3) = 0 Then
                    offset = 0
                End If
                _paletteRam(offset) = value
            End If
        End Sub
        
        Public Function ReadRegister(address As UShort) As Byte
            Select Case address And &H7US
                Case &H2US ' PPUSTATUS
                    Dim status As Byte = _ppuStatus
                    _ppuStatus = CByte(_ppuStatus And (Not &H80)) ' Clear VBlank flag
                    _addressLatch = False
                    Return status
                Case &H4US ' OAMDATA
                    Return 0
                Case &H7US ' PPUDATA
                    Dim data As Byte = _dataBuffer
                    _dataBuffer = ReadVRAM(_ppuAddr)
                    ' Palette reads are immediate
                    If _ppuAddr >= &H3F00US Then
                        data = _dataBuffer
                    End If
                    ' Increment address
                    Dim increment As UShort = If((_ppuCtrl And &H4) <> 0, 32US, 1US)
                    _ppuAddr = CUShort(_ppuAddr + increment)
                    Return data
                Case Else
                    Return 0
            End Select
        End Function
        
        Public Sub WriteRegister(address As UShort, value As Byte)
            Select Case address And &H7US
                Case &H0US ' PPUCTRL
                    _ppuCtrl = value
                Case &H1US ' PPUMASK
                    _ppuMask = value
                Case &H3US ' OAMADDR
                    _oamAddr = value
                Case &H4US ' OAMDATA
                    ' OAM data write (sprites) - not implemented
                Case &H5US ' PPUSCROLL
                    ' Note: Simplified implementation - scroll values not stored
                    ' Full implementation would store X/Y scroll and use for rendering
                    _addressLatch = Not _addressLatch
                Case &H6US ' PPUADDR
                    If Not _addressLatch Then
                        ' First write: high byte (clear low byte, set high byte)
                        _ppuAddr = CUShort((CUShort(value) << 8) Or (_ppuAddr And &H00FFUS))
                    Else
                        ' Second write: low byte (clear high byte of previous high write, set low byte)
                        _ppuAddr = CUShort((_ppuAddr And &HFF00US) Or value)
                    End If
                    _addressLatch = Not _addressLatch
                Case &H7US ' PPUDATA
                    WriteVRAM(_ppuAddr, value)
                    Dim increment As UShort = If((_ppuCtrl And &H4) <> 0, 32US, 1US)
                    _ppuAddr = CUShort(_ppuAddr + increment)
            End Select
        End Sub
    End Class

    Public NotInheritable Class Memory
        Private ReadOnly _ram(2047) As Byte           ' 2KB internal RAM
        Private _prgRom As Byte()                     ' PRG-ROM from cartridge
        Private _chrRom As Byte()                     ' CHR-ROM from cartridge
        Private _mapper As Integer                    ' Mapper number
        Private _ppu As PPU                           ' Reference to PPU for register access

        Public Sub New()
            _prgRom = Array.Empty(Of Byte)()
            _chrRom = Array.Empty(Of Byte)()
        End Sub
        
        Public Sub SetPPU(ppu As PPU)
            _ppu = ppu
        End Sub

        Public Sub LoadCartridge(romData As Byte())
            ' Parse iNES format
            ' Header: 16 bytes
            If romData.Length < 16 Then
                Throw New InvalidOperationException("Invalid NES ROM file")
            End If

            ' Check for "NES" + MS-DOS EOF marker (0x4E 0x45 0x53 0x1A)
            Const INesSignature1 As Byte = &H4E  ' 'N'
            Const INesSignature2 As Byte = &H45  ' 'E'
            Const INesSignature3 As Byte = &H53  ' 'S'
            Const INesSignature4 As Byte = &H1A  ' MS-DOS EOF
            
            If romData(0) <> INesSignature1 OrElse romData(1) <> INesSignature2 OrElse 
               romData(2) <> INesSignature3 OrElse romData(3) <> INesSignature4 Then
                Throw New InvalidOperationException("Not a valid iNES ROM file")
            End If

            Dim prgSize As Integer = romData(4) * 16384  ' 16KB units
            Dim chrSize As Integer = romData(5) * 8192   ' 8KB units
            _mapper = (romData(6) >> 4) Or (romData(7) And &HF0)

            ' Validate ROM size
            Dim expectedSize As Integer = 16 + prgSize + chrSize
            If romData.Length < expectedSize Then
                Throw New InvalidOperationException($"ROM file is too small. Expected at least {expectedSize} bytes, got {romData.Length} bytes.")
            End If

            ' Load PRG-ROM
            If prgSize > 0 Then
                ReDim _prgRom(prgSize - 1)
                Buffer.BlockCopy(romData, 16, _prgRom, 0, prgSize)
            End If

            ' Load CHR-ROM
            If chrSize > 0 Then
                ReDim _chrRom(chrSize - 1)
                Buffer.BlockCopy(romData, 16 + prgSize, _chrRom, 0, chrSize)
            End If
        End Sub

        Public Function Read(address As UShort) As Byte
            If address < &H2000US Then
                ' Internal RAM (mirrored every 2KB)
                Return _ram(address And &H7FFUS)
            ElseIf address < &H4000US Then
                ' PPU registers ($2000-$3FFF, mirrored every 8 bytes)
                If _ppu IsNot Nothing Then
                    Return _ppu.ReadRegister(address)
                End If
                Return 0
            ElseIf address < &H4020US Then
                ' APU and I/O registers
                Return 0
            ElseIf address >= &H8000US Then
                ' PRG-ROM
                If _prgRom IsNot Nothing AndAlso _prgRom.Length > 0 Then
                    Dim romAddr As Integer = address - &H8000US
                    If _prgRom.Length = 16384 Then
                        ' 16KB ROM, mirror to both banks
                        romAddr = romAddr And &H3FFF
                    End If
                    If romAddr < _prgRom.Length Then
                        Return _prgRom(romAddr)
                    End If
                End If
                Return 0
            Else
                Return 0
            End If
        End Function
        
        Public Function ReadCHR(address As UShort) As Byte
            ' Read from CHR-ROM (pattern tables)
            If _chrRom IsNot Nothing AndAlso address < _chrRom.Length Then
                Return _chrRom(address)
            End If
            Return 0
        End Function

        Public Sub Write(address As UShort, value As Byte)
            If address < &H2000US Then
                ' Internal RAM (mirrored every 2KB)
                _ram(address And &H7FFUS) = value
            ElseIf address < &H4000US Then
                ' PPU registers ($2000-$3FFF, mirrored every 8 bytes)
                If _ppu IsNot Nothing Then
                    _ppu.WriteRegister(address, value)
                End If
            ElseIf address < &H4020US Then
                ' APU and I/O registers
            ElseIf address >= &H6000US AndAlso address < &H8000US Then
                ' SRAM (not implemented)
            End If
        End Sub
    End Class
End Class
