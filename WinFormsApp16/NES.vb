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
        _memory.SetControllers(Controller1, Controller2)
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
                
                ' Check for NMI (triggered when VBlank starts and PPUCTRL bit 7 is set)
                If _ppu.NmiTriggered Then
                    _ppu.NmiTriggered = False
                    _cpu.TriggerNMI()
                End If
                
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

        Public Sub TriggerNMI()
            ' NMI (Non-Maskable Interrupt)
            ' Push PC and status to stack, then jump to NMI vector
            Push(CByte(PC >> 8))
            Push(CByte(PC And &HFF))
            Push(CByte(P And (Not FlagB)))
            SetFlag(FlagI, True)
            PC = CUShort(_memory.Read(&HFFFA) Or (CUShort(_memory.Read(&HFFFB)) << 8))
        End Sub

        Public Function ExecuteInstruction() As Integer
            ' Full 6502 instruction execution
            ' Returns number of cycles taken
            Dim opcode As Byte = _memory.Read(PC)
            PC = CUShort(PC + 1US)

            Select Case opcode
                ' ADC - Add with Carry
                Case &H69 ' ADC Immediate
                    PC = CUShort(PC + 1US) : Return ADC(_memory.Read(CUShort(PC - 1US)), 2)
                Case &H65 ' ADC Zero Page
                    PC = CUShort(PC + 1US) : Return ADC(_memory.Read(_memory.Read(CUShort(PC - 1US))), 3)
                Case &H75 ' ADC Zero Page,X
                    PC = CUShort(PC + 1US) : Return ADC(_memory.Read(CByte((_memory.Read(CUShort(PC - 1US)) + X) And &HFF)), 4)
                Case &H6D ' ADC Absolute
                    Dim addr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    Return ADC(_memory.Read(addr), 4)
                Case &H7D ' ADC Absolute,X
                    Dim addr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    Dim pageCross As Boolean = (addr And &HFF00) <> ((addr + X) And &HFF00)
                    Return ADC(_memory.Read(CUShort(addr + X)), If(pageCross, 5, 4))
                Case &H79 ' ADC Absolute,Y
                    Dim addr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    Dim pageCross As Boolean = (addr And &HFF00) <> ((addr + Y) And &HFF00)
                    Return ADC(_memory.Read(CUShort(addr + Y)), If(pageCross, 5, 4))
                Case &H61 ' ADC (Indirect,X)
                    Dim result As UShort = IndirectX()
                    PC = CUShort(PC + 1US)
                    Return ADC(_memory.Read(result), 6)
                Case &H71 ' ADC (Indirect),Y
                    Dim baseAddr As UShort = IndirectY()
                    Dim pageCross As Boolean = ((baseAddr - Y) And &HFF00) <> (baseAddr And &HFF00)
                    PC = CUShort(PC + 1US)
                    Return ADC(_memory.Read(baseAddr), If(pageCross, 6, 5))

                ' AND - Logical AND
                Case &H29 ' AND Immediate
                    A = CByte(A And _memory.Read(PC)) : PC = CUShort(PC + 1US) : SetZN(A) : Return 2
                Case &H25 ' AND Zero Page
                    A = CByte(A And _memory.Read(_memory.Read(PC))) : PC = CUShort(PC + 1US) : SetZN(A) : Return 3
                Case &H35 ' AND Zero Page,X
                    A = CByte(A And _memory.Read(CByte((_memory.Read(PC) + X) And &HFF))) : PC = CUShort(PC + 1US) : SetZN(A) : Return 4
                Case &H2D ' AND Absolute
                    Dim addr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    A = CByte(A And _memory.Read(addr)) : SetZN(A) : Return 4
                Case &H3D ' AND Absolute,X
                    Dim addr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    Dim pageCross As Boolean = (addr And &HFF00) <> ((addr + X) And &HFF00)
                    A = CByte(A And _memory.Read(CUShort(addr + X))) : SetZN(A) : Return If(pageCross, 5, 4)
                Case &H39 ' AND Absolute,Y
                    Dim addr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    Dim pageCross As Boolean = (addr And &HFF00) <> ((addr + Y) And &HFF00)
                    A = CByte(A And _memory.Read(CUShort(addr + Y))) : SetZN(A) : Return If(pageCross, 5, 4)
                Case &H21 ' AND (Indirect,X)
                    A = CByte(A And _memory.Read(IndirectX())) : PC = CUShort(PC + 1US) : SetZN(A) : Return 6
                Case &H31 ' AND (Indirect),Y
                    Dim baseAddr As UShort = IndirectY()
                    Dim pageCross As Boolean = ((baseAddr - Y) And &HFF00) <> (baseAddr And &HFF00)
                    A = CByte(A And _memory.Read(baseAddr)) : PC = CUShort(PC + 1US) : SetZN(A) : Return If(pageCross, 6, 5)

                ' ASL - Arithmetic Shift Left
                Case &H0A ' ASL Accumulator
                    SetFlag(FlagC, (A And &H80) <> 0) : A = CByte((A << 1) And &HFF) : SetZN(A) : Return 2
                Case &H06 ' ASL Zero Page
                    Dim addr As Byte = _memory.Read(PC) : PC = CUShort(PC + 1US)
                    Dim value As Byte = _memory.Read(addr)
                    SetFlag(FlagC, (value And &H80) <> 0) : value = CByte((value << 1) And &HFF)
                    _memory.Write(addr, value) : SetZN(value) : Return 5
                Case &H16 ' ASL Zero Page,X
                    Dim addr As Byte = CByte((_memory.Read(PC) + X) And &HFF) : PC = CUShort(PC + 1US)
                    Dim value As Byte = _memory.Read(addr)
                    SetFlag(FlagC, (value And &H80) <> 0) : value = CByte((value << 1) And &HFF)
                    _memory.Write(addr, value) : SetZN(value) : Return 6
                Case &H0E ' ASL Absolute
                    Dim addr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    Dim value As Byte = _memory.Read(addr)
                    SetFlag(FlagC, (value And &H80) <> 0) : value = CByte((value << 1) And &HFF)
                    _memory.Write(addr, value) : SetZN(value) : Return 6
                Case &H1E ' ASL Absolute,X
                    Dim addr As UShort = CUShort(ReadWord(PC) + X) : PC = CUShort(PC + 2US)
                    Dim value As Byte = _memory.Read(addr)
                    SetFlag(FlagC, (value And &H80) <> 0) : value = CByte((value << 1) And &HFF)
                    _memory.Write(addr, value) : SetZN(value) : Return 7

                ' BIT - Bit Test
                Case &H24 ' BIT Zero Page
                    Dim value As Byte = _memory.Read(_memory.Read(PC)) : PC = CUShort(PC + 1US)
                    SetFlag(FlagZ, (A And value) = 0) : SetFlag(FlagV, (value And &H40) <> 0) : SetFlag(FlagN, (value And &H80) <> 0) : Return 3
                Case &H2C ' BIT Absolute
                    Dim addr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    Dim value As Byte = _memory.Read(addr)
                    SetFlag(FlagZ, (A And value) = 0) : SetFlag(FlagV, (value And &H40) <> 0) : SetFlag(FlagN, (value And &H80) <> 0) : Return 4

                ' Branch Instructions
                Case &H10 ' BPL - Branch if Positive
                    Return Branch((P And FlagN) = 0)
                Case &H30 ' BMI - Branch if Minus
                    Return Branch((P And FlagN) <> 0)
                Case &H50 ' BVC - Branch if Overflow Clear
                    Return Branch((P And FlagV) = 0)
                Case &H70 ' BVS - Branch if Overflow Set
                    Return Branch((P And FlagV) <> 0)
                Case &H90 ' BCC - Branch if Carry Clear
                    Return Branch((P And FlagC) = 0)
                Case &HB0 ' BCS - Branch if Carry Set
                    Return Branch((P And FlagC) <> 0)
                Case &HD0 ' BNE - Branch if Not Equal
                    Return Branch((P And FlagZ) = 0)
                Case &HF0 ' BEQ - Branch if Equal
                    Return Branch((P And FlagZ) <> 0)

                ' BRK - Force Interrupt
                Case &H0 ' BRK
                    PC = CUShort(PC + 1US)
                    Push(CByte(PC >> 8)) : Push(CByte(PC And &HFF))
                    Push(CByte(P Or FlagB)) : SetFlag(FlagI, True)
                    PC = CUShort(_memory.Read(&HFFFE) Or (CUShort(_memory.Read(&HFFFF)) << 8))
                    Return 7

                ' CMP, CPX, CPY - Compare
                Case &HC9 ' CMP Immediate
                    Compare(A, _memory.Read(PC)) : PC = CUShort(PC + 1US) : Return 2
                Case &HC5 ' CMP Zero Page
                    Compare(A, _memory.Read(_memory.Read(PC))) : PC = CUShort(PC + 1US) : Return 3
                Case &HD5 ' CMP Zero Page,X
                    Compare(A, _memory.Read(CByte((_memory.Read(PC) + X) And &HFF))) : PC = CUShort(PC + 1US) : Return 4
                Case &HCD ' CMP Absolute
                    Dim addr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    Compare(A, _memory.Read(addr)) : Return 4
                Case &HDD ' CMP Absolute,X
                    Dim addr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    Dim pageCross As Boolean = (addr And &HFF00) <> ((addr + X) And &HFF00)
                    Compare(A, _memory.Read(CUShort(addr + X))) : Return If(pageCross, 5, 4)
                Case &HD9 ' CMP Absolute,Y
                    Dim addr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    Dim pageCross As Boolean = (addr And &HFF00) <> ((addr + Y) And &HFF00)
                    Compare(A, _memory.Read(CUShort(addr + Y))) : Return If(pageCross, 5, 4)
                Case &HC1 ' CMP (Indirect,X)
                    Compare(A, _memory.Read(IndirectX())) : PC = CUShort(PC + 1US) : Return 6
                Case &HD1 ' CMP (Indirect),Y
                    Dim baseAddr As UShort = IndirectY()
                    Dim pageCross As Boolean = ((baseAddr - Y) And &HFF00) <> (baseAddr And &HFF00)
                    Compare(A, _memory.Read(baseAddr)) : PC = CUShort(PC + 1US) : Return If(pageCross, 6, 5)
                Case &HE0 ' CPX Immediate
                    Compare(X, _memory.Read(PC)) : PC = CUShort(PC + 1US) : Return 2
                Case &HE4 ' CPX Zero Page
                    Compare(X, _memory.Read(_memory.Read(PC))) : PC = CUShort(PC + 1US) : Return 3
                Case &HEC ' CPX Absolute
                    Dim addr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    Compare(X, _memory.Read(addr)) : Return 4
                Case &HC0 ' CPY Immediate
                    Compare(Y, _memory.Read(PC)) : PC = CUShort(PC + 1US) : Return 2
                Case &HC4 ' CPY Zero Page
                    Compare(Y, _memory.Read(_memory.Read(PC))) : PC = CUShort(PC + 1US) : Return 3
                Case &HCC ' CPY Absolute
                    Dim addr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    Compare(Y, _memory.Read(addr)) : Return 4

                ' DEC, DEX, DEY - Decrement
                Case &HC6 ' DEC Zero Page
                    Dim addr As Byte = _memory.Read(PC) : PC = CUShort(PC + 1US)
                    Dim value As Byte = CByte((_memory.Read(addr) - 1) And &HFF)
                    _memory.Write(addr, value) : SetZN(value) : Return 5
                Case &HD6 ' DEC Zero Page,X
                    Dim addr As Byte = CByte((_memory.Read(PC) + X) And &HFF) : PC = CUShort(PC + 1US)
                    Dim value As Byte = CByte((_memory.Read(addr) - 1) And &HFF)
                    _memory.Write(addr, value) : SetZN(value) : Return 6
                Case &HCE ' DEC Absolute
                    Dim addr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    Dim value As Byte = CByte((_memory.Read(addr) - 1) And &HFF)
                    _memory.Write(addr, value) : SetZN(value) : Return 6
                Case &HDE ' DEC Absolute,X
                    Dim addr As UShort = CUShort(ReadWord(PC) + X) : PC = CUShort(PC + 2US)
                    Dim value As Byte = CByte((_memory.Read(addr) - 1) And &HFF)
                    _memory.Write(addr, value) : SetZN(value) : Return 7
                Case &HCA ' DEX
                    X = CByte((X - 1) And &HFF) : SetZN(X) : Return 2
                Case &H88 ' DEY
                    Y = CByte((Y - 1) And &HFF) : SetZN(Y) : Return 2

                ' EOR - Exclusive OR
                Case &H49 ' EOR Immediate
                    A = CByte(A Xor _memory.Read(PC)) : PC = CUShort(PC + 1US) : SetZN(A) : Return 2
                Case &H45 ' EOR Zero Page
                    A = CByte(A Xor _memory.Read(_memory.Read(PC))) : PC = CUShort(PC + 1US) : SetZN(A) : Return 3
                Case &H55 ' EOR Zero Page,X
                    A = CByte(A Xor _memory.Read(CByte((_memory.Read(PC) + X) And &HFF))) : PC = CUShort(PC + 1US) : SetZN(A) : Return 4
                Case &H4D ' EOR Absolute
                    Dim addr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    A = CByte(A Xor _memory.Read(addr)) : SetZN(A) : Return 4
                Case &H5D ' EOR Absolute,X
                    Dim addr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    Dim pageCross As Boolean = (addr And &HFF00) <> ((addr + X) And &HFF00)
                    A = CByte(A Xor _memory.Read(CUShort(addr + X))) : SetZN(A) : Return If(pageCross, 5, 4)
                Case &H59 ' EOR Absolute,Y
                    Dim addr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    Dim pageCross As Boolean = (addr And &HFF00) <> ((addr + Y) And &HFF00)
                    A = CByte(A Xor _memory.Read(CUShort(addr + Y))) : SetZN(A) : Return If(pageCross, 5, 4)
                Case &H41 ' EOR (Indirect,X)
                    A = CByte(A Xor _memory.Read(IndirectX())) : PC = CUShort(PC + 1US) : SetZN(A) : Return 6
                Case &H51 ' EOR (Indirect),Y
                    Dim baseAddr As UShort = IndirectY()
                    Dim pageCross As Boolean = ((baseAddr - Y) And &HFF00) <> (baseAddr And &HFF00)
                    A = CByte(A Xor _memory.Read(baseAddr)) : PC = CUShort(PC + 1US) : SetZN(A) : Return If(pageCross, 6, 5)

                ' Flag Instructions
                Case &H18 ' CLC
                    SetFlag(FlagC, False) : Return 2
                Case &H38 ' SEC
                    SetFlag(FlagC, True) : Return 2
                Case &H58 ' CLI
                    SetFlag(FlagI, False) : Return 2
                Case &H78 ' SEI
                    SetFlag(FlagI, True) : Return 2
                Case &HB8 ' CLV
                    SetFlag(FlagV, False) : Return 2
                Case &HD8 ' CLD
                    SetFlag(FlagD, False) : Return 2
                Case &HF8 ' SED
                    SetFlag(FlagD, True) : Return 2

                ' INC, INX, INY - Increment
                Case &HE6 ' INC Zero Page
                    Dim addr As Byte = _memory.Read(PC) : PC = CUShort(PC + 1US)
                    Dim value As Byte = CByte((_memory.Read(addr) + 1) And &HFF)
                    _memory.Write(addr, value) : SetZN(value) : Return 5
                Case &HF6 ' INC Zero Page,X
                    Dim addr As Byte = CByte((_memory.Read(PC) + X) And &HFF) : PC = CUShort(PC + 1US)
                    Dim value As Byte = CByte((_memory.Read(addr) + 1) And &HFF)
                    _memory.Write(addr, value) : SetZN(value) : Return 6
                Case &HEE ' INC Absolute
                    Dim addr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    Dim value As Byte = CByte((_memory.Read(addr) + 1) And &HFF)
                    _memory.Write(addr, value) : SetZN(value) : Return 6
                Case &HFE ' INC Absolute,X
                    Dim addr As UShort = CUShort(ReadWord(PC) + X) : PC = CUShort(PC + 2US)
                    Dim value As Byte = CByte((_memory.Read(addr) + 1) And &HFF)
                    _memory.Write(addr, value) : SetZN(value) : Return 7
                Case &HE8 ' INX
                    X = CByte((X + 1) And &HFF) : SetZN(X) : Return 2
                Case &HC8 ' INY
                    Y = CByte((Y + 1) And &HFF) : SetZN(Y) : Return 2

                ' JMP - Jump
                Case &H4C ' JMP Absolute
                    PC = ReadWord(PC) : Return 3
                Case &H6C ' JMP Indirect
                    Dim addr As UShort = ReadWord(PC)
                    ' 6502 bug: if addr is on page boundary, wrap within page
                    If (addr And &HFF) = &HFF Then
                        PC = CUShort(_memory.Read(addr) Or (CUShort(_memory.Read(addr And &HFF00US)) << 8))
                    Else
                        PC = CUShort(_memory.Read(addr) Or (CUShort(_memory.Read(CUShort(addr + 1US))) << 8))
                    End If
                    Return 5

                ' JSR, RTS - Subroutine
                Case &H20 ' JSR
                    Dim target As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    Dim returnAddr As UShort = CUShort(PC - 1US)
                    Push(CByte(returnAddr >> 8)) : Push(CByte(returnAddr And &HFF))
                    PC = target : Return 6
                Case &H60 ' RTS
                    Dim lo As Byte = Pop() : Dim hi As Byte = Pop()
                    PC = CUShort(((CUShort(hi) << 8) Or lo) + 1US) : Return 6

                ' LDA - Load Accumulator
                Case &HA9 ' LDA Immediate
                    A = _memory.Read(PC) : PC = CUShort(PC + 1US) : SetZN(A) : Return 2
                Case &HA5 ' LDA Zero Page
                    A = _memory.Read(_memory.Read(PC)) : PC = CUShort(PC + 1US) : SetZN(A) : Return 3
                Case &HB5 ' LDA Zero Page,X
                    A = _memory.Read(CByte((_memory.Read(PC) + X) And &HFF)) : PC = CUShort(PC + 1US) : SetZN(A) : Return 4
                Case &HAD ' LDA Absolute
                    Dim addr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    A = _memory.Read(addr) : SetZN(A) : Return 4
                Case &HBD ' LDA Absolute,X
                    Dim addr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    Dim pageCross As Boolean = (addr And &HFF00) <> ((addr + X) And &HFF00)
                    A = _memory.Read(CUShort(addr + X)) : SetZN(A) : Return If(pageCross, 5, 4)
                Case &HB9 ' LDA Absolute,Y
                    Dim addr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    Dim pageCross As Boolean = (addr And &HFF00) <> ((addr + Y) And &HFF00)
                    A = _memory.Read(CUShort(addr + Y)) : SetZN(A) : Return If(pageCross, 5, 4)
                Case &HA1 ' LDA (Indirect,X)
                    A = _memory.Read(IndirectX()) : PC = CUShort(PC + 1US) : SetZN(A) : Return 6
                Case &HB1 ' LDA (Indirect),Y
                    Dim baseAddr As UShort = IndirectY()
                    Dim pageCross As Boolean = ((baseAddr - Y) And &HFF00) <> (baseAddr And &HFF00)
                    A = _memory.Read(baseAddr) : PC = CUShort(PC + 1US) : SetZN(A) : Return If(pageCross, 6, 5)

                ' LDX - Load X
                Case &HA2 ' LDX Immediate
                    X = _memory.Read(PC) : PC = CUShort(PC + 1US) : SetZN(X) : Return 2
                Case &HA6 ' LDX Zero Page
                    X = _memory.Read(_memory.Read(PC)) : PC = CUShort(PC + 1US) : SetZN(X) : Return 3
                Case &HB6 ' LDX Zero Page,Y
                    X = _memory.Read(CByte((_memory.Read(PC) + Y) And &HFF)) : PC = CUShort(PC + 1US) : SetZN(X) : Return 4
                Case &HAE ' LDX Absolute
                    Dim addr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    X = _memory.Read(addr) : SetZN(X) : Return 4
                Case &HBE ' LDX Absolute,Y
                    Dim addr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    Dim pageCross As Boolean = (addr And &HFF00) <> ((addr + Y) And &HFF00)
                    X = _memory.Read(CUShort(addr + Y)) : SetZN(X) : Return If(pageCross, 5, 4)

                ' LDY - Load Y
                Case &HA0 ' LDY Immediate
                    Y = _memory.Read(PC) : PC = CUShort(PC + 1US) : SetZN(Y) : Return 2
                Case &HA4 ' LDY Zero Page
                    Y = _memory.Read(_memory.Read(PC)) : PC = CUShort(PC + 1US) : SetZN(Y) : Return 3
                Case &HB4 ' LDY Zero Page,X
                    Y = _memory.Read(CByte((_memory.Read(PC) + X) And &HFF)) : PC = CUShort(PC + 1US) : SetZN(Y) : Return 4
                Case &HAC ' LDY Absolute
                    Dim addr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    Y = _memory.Read(addr) : SetZN(Y) : Return 4
                Case &HBC ' LDY Absolute,X
                    Dim addr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    Dim pageCross As Boolean = (addr And &HFF00) <> ((addr + X) And &HFF00)
                    Y = _memory.Read(CUShort(addr + X)) : SetZN(Y) : Return If(pageCross, 5, 4)

                ' LSR - Logical Shift Right
                Case &H4A ' LSR Accumulator
                    SetFlag(FlagC, (A And &H1) <> 0) : A = CByte(A >> 1) : SetZN(A) : Return 2
                Case &H46 ' LSR Zero Page
                    Dim addr As Byte = _memory.Read(PC) : PC = CUShort(PC + 1US)
                    Dim value As Byte = _memory.Read(addr)
                    SetFlag(FlagC, (value And &H1) <> 0) : value = CByte(value >> 1)
                    _memory.Write(addr, value) : SetZN(value) : Return 5
                Case &H56 ' LSR Zero Page,X
                    Dim addr As Byte = CByte((_memory.Read(PC) + X) And &HFF) : PC = CUShort(PC + 1US)
                    Dim value As Byte = _memory.Read(addr)
                    SetFlag(FlagC, (value And &H1) <> 0) : value = CByte(value >> 1)
                    _memory.Write(addr, value) : SetZN(value) : Return 6
                Case &H4E ' LSR Absolute
                    Dim addr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    Dim value As Byte = _memory.Read(addr)
                    SetFlag(FlagC, (value And &H1) <> 0) : value = CByte(value >> 1)
                    _memory.Write(addr, value) : SetZN(value) : Return 6
                Case &H5E ' LSR Absolute,X
                    Dim addr As UShort = CUShort(ReadWord(PC) + X) : PC = CUShort(PC + 2US)
                    Dim value As Byte = _memory.Read(addr)
                    SetFlag(FlagC, (value And &H1) <> 0) : value = CByte(value >> 1)
                    _memory.Write(addr, value) : SetZN(value) : Return 7

                ' NOP - No Operation
                Case &HEA ' NOP
                    Return 2

                ' ORA - Logical OR
                Case &H09 ' ORA Immediate
                    A = CByte(A Or _memory.Read(PC)) : PC = CUShort(PC + 1US) : SetZN(A) : Return 2
                Case &H05 ' ORA Zero Page
                    A = CByte(A Or _memory.Read(_memory.Read(PC))) : PC = CUShort(PC + 1US) : SetZN(A) : Return 3
                Case &H15 ' ORA Zero Page,X
                    A = CByte(A Or _memory.Read(CByte((_memory.Read(PC) + X) And &HFF))) : PC = CUShort(PC + 1US) : SetZN(A) : Return 4
                Case &H0D ' ORA Absolute
                    Dim addr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    A = CByte(A Or _memory.Read(addr)) : SetZN(A) : Return 4
                Case &H1D ' ORA Absolute,X
                    Dim addr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    Dim pageCross As Boolean = (addr And &HFF00) <> ((addr + X) And &HFF00)
                    A = CByte(A Or _memory.Read(CUShort(addr + X))) : SetZN(A) : Return If(pageCross, 5, 4)
                Case &H19 ' ORA Absolute,Y
                    Dim addr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    Dim pageCross As Boolean = (addr And &HFF00) <> ((addr + Y) And &HFF00)
                    A = CByte(A Or _memory.Read(CUShort(addr + Y))) : SetZN(A) : Return If(pageCross, 5, 4)
                Case &H01 ' ORA (Indirect,X)
                    A = CByte(A Or _memory.Read(IndirectX())) : PC = CUShort(PC + 1US) : SetZN(A) : Return 6
                Case &H11 ' ORA (Indirect),Y
                    Dim baseAddr As UShort = IndirectY()
                    Dim pageCross As Boolean = ((baseAddr - Y) And &HFF00) <> (baseAddr And &HFF00)
                    A = CByte(A Or _memory.Read(baseAddr)) : PC = CUShort(PC + 1US) : SetZN(A) : Return If(pageCross, 6, 5)

                ' Register Instructions
                Case &HAA ' TAX
                    X = A : SetZN(X) : Return 2
                Case &H8A ' TXA
                    A = X : SetZN(A) : Return 2
                Case &HA8 ' TAY
                    Y = A : SetZN(Y) : Return 2
                Case &H98 ' TYA
                    A = Y : SetZN(A) : Return 2
                Case &HBA ' TSX
                    X = SP : SetZN(X) : Return 2
                Case &H9A ' TXS
                    SP = X : Return 2

                ' ROL - Rotate Left
                Case &H2A ' ROL Accumulator
                    Dim carry As Byte = If((P And FlagC) <> 0, CByte(1), CByte(0))
                    SetFlag(FlagC, (A And &H80) <> 0) : A = CByte(((A << 1) Or carry) And &HFF) : SetZN(A) : Return 2
                Case &H26 ' ROL Zero Page
                    Dim addr As Byte = _memory.Read(PC) : PC = CUShort(PC + 1US)
                    Dim value As Byte = _memory.Read(addr)
                    Dim carry As Byte = If((P And FlagC) <> 0, CByte(1), CByte(0))
                    SetFlag(FlagC, (value And &H80) <> 0) : value = CByte(((value << 1) Or carry) And &HFF)
                    _memory.Write(addr, value) : SetZN(value) : Return 5
                Case &H36 ' ROL Zero Page,X
                    Dim addr As Byte = CByte((_memory.Read(PC) + X) And &HFF) : PC = CUShort(PC + 1US)
                    Dim value As Byte = _memory.Read(addr)
                    Dim carry As Byte = If((P And FlagC) <> 0, CByte(1), CByte(0))
                    SetFlag(FlagC, (value And &H80) <> 0) : value = CByte(((value << 1) Or carry) And &HFF)
                    _memory.Write(addr, value) : SetZN(value) : Return 6
                Case &H2E ' ROL Absolute
                    Dim addr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    Dim value As Byte = _memory.Read(addr)
                    Dim carry As Byte = If((P And FlagC) <> 0, CByte(1), CByte(0))
                    SetFlag(FlagC, (value And &H80) <> 0) : value = CByte(((value << 1) Or carry) And &HFF)
                    _memory.Write(addr, value) : SetZN(value) : Return 6
                Case &H3E ' ROL Absolute,X
                    Dim addr As UShort = CUShort(ReadWord(PC) + X) : PC = CUShort(PC + 2US)
                    Dim value As Byte = _memory.Read(addr)
                    Dim carry As Byte = If((P And FlagC) <> 0, CByte(1), CByte(0))
                    SetFlag(FlagC, (value And &H80) <> 0) : value = CByte(((value << 1) Or carry) And &HFF)
                    _memory.Write(addr, value) : SetZN(value) : Return 7

                ' ROR - Rotate Right
                Case &H6A ' ROR Accumulator
                    Dim carry As Byte = If((P And FlagC) <> 0, CByte(&H80), CByte(0))
                    SetFlag(FlagC, (A And &H1) <> 0) : A = CByte((A >> 1) Or carry) : SetZN(A) : Return 2
                Case &H66 ' ROR Zero Page
                    Dim addr As Byte = _memory.Read(PC) : PC = CUShort(PC + 1US)
                    Dim value As Byte = _memory.Read(addr)
                    Dim carry As Byte = If((P And FlagC) <> 0, CByte(&H80), CByte(0))
                    SetFlag(FlagC, (value And &H1) <> 0) : value = CByte((value >> 1) Or carry)
                    _memory.Write(addr, value) : SetZN(value) : Return 5
                Case &H76 ' ROR Zero Page,X
                    Dim addr As Byte = CByte((_memory.Read(PC) + X) And &HFF) : PC = CUShort(PC + 1US)
                    Dim value As Byte = _memory.Read(addr)
                    Dim carry As Byte = If((P And FlagC) <> 0, CByte(&H80), CByte(0))
                    SetFlag(FlagC, (value And &H1) <> 0) : value = CByte((value >> 1) Or carry)
                    _memory.Write(addr, value) : SetZN(value) : Return 6
                Case &H6E ' ROR Absolute
                    Dim addr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    Dim value As Byte = _memory.Read(addr)
                    Dim carry As Byte = If((P And FlagC) <> 0, CByte(&H80), CByte(0))
                    SetFlag(FlagC, (value And &H1) <> 0) : value = CByte((value >> 1) Or carry)
                    _memory.Write(addr, value) : SetZN(value) : Return 6
                Case &H7E ' ROR Absolute,X
                    Dim addr As UShort = CUShort(ReadWord(PC) + X) : PC = CUShort(PC + 2US)
                    Dim value As Byte = _memory.Read(addr)
                    Dim carry As Byte = If((P And FlagC) <> 0, CByte(&H80), CByte(0))
                    SetFlag(FlagC, (value And &H1) <> 0) : value = CByte((value >> 1) Or carry)
                    _memory.Write(addr, value) : SetZN(value) : Return 7

                ' RTI - Return from Interrupt
                Case &H40 ' RTI
                    P = Pop() : Dim lo As Byte = Pop() : Dim hi As Byte = Pop()
                    PC = CUShort((CUShort(hi) << 8) Or lo) : Return 6

                ' SBC - Subtract with Carry
                Case &HE9 ' SBC Immediate
                    PC = CUShort(PC + 1US) : Return SBC(_memory.Read(CUShort(PC - 1US)), 2)
                Case &HE5 ' SBC Zero Page
                    PC = CUShort(PC + 1US) : Return SBC(_memory.Read(_memory.Read(CUShort(PC - 1US))), 3)
                Case &HF5 ' SBC Zero Page,X
                    PC = CUShort(PC + 1US) : Return SBC(_memory.Read(CByte((_memory.Read(CUShort(PC - 1US)) + X) And &HFF)), 4)
                Case &HED ' SBC Absolute
                    Dim addr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    Return SBC(_memory.Read(addr), 4)
                Case &HFD ' SBC Absolute,X
                    Dim addr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    Dim pageCross As Boolean = (addr And &HFF00) <> ((addr + X) And &HFF00)
                    Return SBC(_memory.Read(CUShort(addr + X)), If(pageCross, 5, 4))
                Case &HF9 ' SBC Absolute,Y
                    Dim addr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    Dim pageCross As Boolean = (addr And &HFF00) <> ((addr + Y) And &HFF00)
                    Return SBC(_memory.Read(CUShort(addr + Y)), If(pageCross, 5, 4))
                Case &HE1 ' SBC (Indirect,X)
                    Dim result As UShort = IndirectX()
                    PC = CUShort(PC + 1US)
                    Return SBC(_memory.Read(result), 6)
                Case &HF1 ' SBC (Indirect),Y
                    Dim baseAddr As UShort = IndirectY()
                    Dim pageCross As Boolean = ((baseAddr - Y) And &HFF00) <> (baseAddr And &HFF00)
                    PC = CUShort(PC + 1US)
                    Return SBC(_memory.Read(baseAddr), If(pageCross, 6, 5))

                ' STA - Store Accumulator
                Case &H85 ' STA Zero Page
                    Dim zpAddr As Byte = _memory.Read(PC) : PC = CUShort(PC + 1US)
                    _memory.Write(zpAddr, A) : Return 3
                Case &H95 ' STA Zero Page,X
                    Dim zpAddr As Byte = CByte((_memory.Read(PC) + X) And &HFF) : PC = CUShort(PC + 1US)
                    _memory.Write(zpAddr, A) : Return 4
                Case &H8D ' STA Absolute
                    Dim absAddr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    _memory.Write(absAddr, A) : Return 4
                Case &H9D ' STA Absolute,X
                    Dim addr As UShort = CUShort(ReadWord(PC) + X) : PC = CUShort(PC + 2US)
                    _memory.Write(addr, A) : Return 5
                Case &H99 ' STA Absolute,Y
                    Dim addr As UShort = CUShort(ReadWord(PC) + Y) : PC = CUShort(PC + 2US)
                    _memory.Write(addr, A) : Return 5
                Case &H81 ' STA (Indirect,X)
                    _memory.Write(IndirectX(), A) : PC = CUShort(PC + 1US) : Return 6
                Case &H91 ' STA (Indirect),Y
                    _memory.Write(IndirectY(), A) : PC = CUShort(PC + 1US) : Return 6

                ' Stack Instructions
                Case &H48 ' PHA
                    Push(A) : Return 3
                Case &H68 ' PLA
                    A = Pop() : SetZN(A) : Return 4
                Case &H08 ' PHP
                    Push(CByte(P Or FlagB Or FlagU)) : Return 3
                Case &H28 ' PLP
                    P = CByte((Pop() And (Not FlagB)) Or FlagU) : Return 4

                ' STX - Store X
                Case &H86 ' STX Zero Page
                    Dim zpAddr As Byte = _memory.Read(PC) : PC = CUShort(PC + 1US)
                    _memory.Write(zpAddr, X) : Return 3
                Case &H96 ' STX Zero Page,Y
                    Dim zpAddr As Byte = CByte((_memory.Read(PC) + Y) And &HFF) : PC = CUShort(PC + 1US)
                    _memory.Write(zpAddr, X) : Return 4
                Case &H8E ' STX Absolute
                    Dim absAddr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    _memory.Write(absAddr, X) : Return 4

                ' STY - Store Y
                Case &H84 ' STY Zero Page
                    Dim zpAddr As Byte = _memory.Read(PC) : PC = CUShort(PC + 1US)
                    _memory.Write(zpAddr, Y) : Return 3
                Case &H94 ' STY Zero Page,X
                    Dim zpAddr As Byte = CByte((_memory.Read(PC) + X) And &HFF) : PC = CUShort(PC + 1US)
                    _memory.Write(zpAddr, Y) : Return 4
                Case &H8C ' STY Absolute
                    Dim absAddr As UShort = ReadWord(PC) : PC = CUShort(PC + 2US)
                    _memory.Write(absAddr, Y) : Return 4

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
            _memory.Write(CUShort(SP) + &H100US, value)
            SP = CByte((SP - 1) And &HFF)
        End Sub

        Private Function Pop() As Byte
            SP = CByte((SP + 1) And &HFF)
            Return _memory.Read(CUShort(SP) + &H100US)
        End Function

        Private Sub SetFlag(flag As Byte, condition As Boolean)
            If condition Then
                P = CByte(P Or flag)
            Else
                P = CByte(P And (Not flag))
            End If
        End Sub

        Private Sub Compare(reg As Byte, value As Byte)
            Dim result As Integer = reg - value
            SetFlag(FlagC, result >= 0)
            SetFlag(FlagZ, (result And &HFF) = 0)
            SetFlag(FlagN, (result And &H80) <> 0)
        End Sub

        Private Function ADC(value As Byte, cycles As Integer) As Integer
            Dim sum As Integer = A + value + If((P And FlagC) <> 0, 1, 0)
            SetFlag(FlagC, sum > 255)
            SetFlag(FlagV, ((A Xor value) And &H80) = 0 AndAlso ((A Xor sum) And &H80) <> 0)
            A = CByte(sum And &HFF)
            SetZN(A)
            Return cycles
        End Function

        Private Function SBC(value As Byte, cycles As Integer) As Integer
            ' SBC is equivalent to ADC with inverted value
            Dim invValue As Byte = CByte((Not value) And &HFF)
            Return ADC(invValue, cycles)
        End Function

        Private Function Branch(condition As Boolean) As Integer
            Dim offset As SByte = CSByte(_memory.Read(PC))
            PC = CUShort(PC + 1US)
            If condition Then
                Dim oldPC As UShort = PC
                ' Convert to signed integer to handle negative offsets properly
                Dim newPC As Integer = CInt(oldPC) + CInt(offset)
                ' Wrap around to 16-bit range: handle negative values by adding 65536
                If newPC < 0 Then
                    newPC = newPC + 65536
                ElseIf newPC > 65535 Then
                    newPC = newPC And &HFFFF
                End If
                PC = CUShort(newPC)
                ' Add 1 cycle for branch taken, +1 more if page boundary crossed
                Return If((oldPC And &HFF00) <> (PC And &HFF00), 4, 3)
            End If
            Return 2
        End Function

        Private Function IndirectX() As UShort
            Dim zpAddr As Byte = CByte((_memory.Read(PC) + X) And &HFF)
            Return CUShort(_memory.Read(zpAddr) Or (CUShort(_memory.Read(CByte((zpAddr + 1) And &HFF))) << 8))
        End Function

        Private Function IndirectY() As UShort
            Dim zpAddr As Byte = _memory.Read(PC)
            Dim addr As UShort = CUShort(_memory.Read(zpAddr) Or (CUShort(_memory.Read(CByte((zpAddr + 1) And &HFF))) << 8))
            Return CUShort(addr + Y)
        End Function
    End Class

    Public NotInheritable Class PPU
        Private ReadOnly _memory As Memory
        Private ReadOnly _displayBuffer As Integer()

        Private _cycle As Integer
        Private _scanline As Integer

        Public FrameComplete As Boolean
        Public NmiTriggered As Boolean

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
            NmiTriggered = False
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

            ' Set VBlank flag at scanline 241 and trigger NMI if enabled
            If _scanline = 241 AndAlso _cycle = 1 Then
                _ppuStatus = CByte(_ppuStatus Or &H80) ' Set VBlank flag (bit 7)
                ' Trigger NMI if PPUCTRL bit 7 is set
                If (_ppuCtrl And &H80) <> 0 Then
                    NmiTriggered = True
                End If
            End If

            ' Clear VBlank flag at scanline 261 (pre-render scanline)
            If _scanline = 261 AndAlso _cycle = 1 Then
                _ppuStatus = CByte(_ppuStatus And (Not &H80)) ' Clear VBlank flag
            End If

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
        Private _controller1 As Boolean()             ' Reference to Controller 1 state
        Private _controller2 As Boolean()             ' Reference to Controller 2 state
        Private _controller1Shift As Byte             ' Controller 1 shift register
        Private _controller2Shift As Byte             ' Controller 2 shift register
        Private _controllerStrobe As Boolean          ' Controller strobe state

        Public Sub New()
            _prgRom = Array.Empty(Of Byte)()
            _chrRom = Array.Empty(Of Byte)()
        End Sub
        
        Public Sub SetPPU(ppu As PPU)
            _ppu = ppu
        End Sub

        Public Sub SetControllers(controller1 As Boolean(), controller2 As Boolean())
            _controller1 = controller1
            _controller2 = controller2
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
            ElseIf address = &H4016US Then
                ' Controller 1 ($4016)
                If _controller1 IsNot Nothing Then
                    Dim result As Byte = If((_controller1Shift And 1) <> 0, CByte(1), CByte(0))
                    _controller1Shift = CByte(_controller1Shift >> 1)
                    Return result
                End If
                Return 0
            ElseIf address = &H4017US Then
                ' Controller 2 ($4017)
                If _controller2 IsNot Nothing Then
                    Dim result As Byte = If((_controller2Shift And 1) <> 0, CByte(1), CByte(0))
                    _controller2Shift = CByte(_controller2Shift >> 1)
                    Return result
                End If
                Return 0
            ElseIf address < &H4020US Then
                ' Other APU and I/O registers
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
            ElseIf address = &H4016US Then
                ' Controller strobe ($4016)
                Dim wasHigh As Boolean = _controllerStrobe
                _controllerStrobe = (value And 1) <> 0
                
                ' On falling edge of strobe (high to low), latch controller state
                If wasHigh AndAlso Not _controllerStrobe Then
                    ' Load controller 1 state into shift register
                    If _controller1 IsNot Nothing Then
                        _controller1Shift = 0
                        For i As Integer = 0 To 7
                            If _controller1(i) Then
                                _controller1Shift = CByte(_controller1Shift Or (1 << i))
                            End If
                        Next
                    End If
                    
                    ' Load controller 2 state into shift register
                    If _controller2 IsNot Nothing Then
                        _controller2Shift = 0
                        For i As Integer = 0 To 7
                            If _controller2(i) Then
                                _controller2Shift = CByte(_controller2Shift Or (1 << i))
                            End If
                        Next
                    End If
                End If
            ElseIf address < &H4020US Then
                ' Other APU and I/O registers
            ElseIf address >= &H6000US AndAlso address < &H8000US Then
                ' SRAM (not implemented)
            End If
        End Sub
    End Class
End Class
