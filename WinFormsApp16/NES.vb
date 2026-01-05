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
        ' One frame = 29780.5 CPU cycles (262 scanlines * 113.667 cycles/scanline)
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
                    PC = CUShort(PC + 1US)
                    Push(CByte(PC >> 8))
                    Push(CByte(PC And &HFF))
                    PC = target
                    Return 6

                Case &H60 ' RTS
                    Dim lo As Byte = Pop()
                    Dim hi As Byte = Pop()
                    PC = CUShort((CUShort(hi) << 8) Or lo)
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
            SP = CByte(SP - 1)
        End Sub

        Private Function Pop() As Byte
            SP = CByte(SP + 1)
            Return _memory.Read(CUShort(&H100US + SP))
        End Function
    End Class

    Public NotInheritable Class PPU
        Private ReadOnly _memory As Memory
        Private ReadOnly _displayBuffer As Integer()

        Private _cycle As Integer
        Private _scanline As Integer

        Public FrameComplete As Boolean

        Public Sub New(memory As Memory, displayBuffer As Integer())
            _memory = memory
            _displayBuffer = displayBuffer
            Reset()
        End Sub

        Public Sub Reset()
            _cycle = 0
            _scanline = 0
            FrameComplete = False
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
            ' Simplified rendering - just clear to a default color for now
            ' Full NES PPU would read from pattern tables, nametables, etc.
            For i As Integer = 0 To _displayBuffer.Length - 1
                _displayBuffer(i) = &HFF000000 ' Black (ARGB format)
            Next
        End Sub
    End Class

    Public NotInheritable Class Memory
        Private ReadOnly _ram(2047) As Byte           ' 2KB internal RAM
        Private _prgRom As Byte()                     ' PRG-ROM from cartridge
        Private _chrRom As Byte()                     ' CHR-ROM from cartridge
        Private _mapper As Integer                    ' Mapper number

        Public Sub New()
            _prgRom = Array.Empty(Of Byte)()
            _chrRom = Array.Empty(Of Byte)()
        End Sub

        Public Sub LoadCartridge(romData As Byte())
            ' Parse iNES format
            ' Header: 16 bytes
            If romData.Length < 16 Then
                Throw New InvalidOperationException("Invalid NES ROM file")
            End If

            ' Check for "NES" + MS-DOS EOF marker
            If romData(0) <> &H4E OrElse romData(1) <> &H45 OrElse romData(2) <> &H53 OrElse romData(3) <> &H1A Then
                Throw New InvalidOperationException("Not a valid iNES ROM file")
            End If

            Dim prgSize As Integer = romData(4) * 16384  ' 16KB units
            Dim chrSize As Integer = romData(5) * 8192   ' 8KB units
            _mapper = (romData(6) >> 4) Or (romData(7) And &HF0)

            ' Load PRG-ROM
            ReDim _prgRom(prgSize - 1)
            Buffer.BlockCopy(romData, 16, _prgRom, 0, prgSize)

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
                ' PPU registers (not fully implemented)
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

        Public Sub Write(address As UShort, value As Byte)
            If address < &H2000US Then
                ' Internal RAM (mirrored every 2KB)
                _ram(address And &H7FFUS) = value
            ElseIf address < &H4000US Then
                ' PPU registers (not fully implemented)
            ElseIf address < &H4020US Then
                ' APU and I/O registers
            ElseIf address >= &H6000US AndAlso address < &H8000US Then
                ' SRAM (not implemented)
            End If
        End Sub
    End Class
End Class
