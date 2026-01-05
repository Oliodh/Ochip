Option Strict On
Option Explicit On

Imports System.IO

Public NotInheritable Class Chip8
    Public Const DisplayWidthLowRes As Integer = 64
    Public Const DisplayHeightLowRes As Integer = 32
    Public Const DisplayWidthHighRes As Integer = 128
    Public Const DisplayHeightHighRes As Integer = 64
    Public Const ProgramStart As UShort = &H200US

    Private ReadOnly _rng As New Random()

    Public ReadOnly Memory(4095) As Byte
    Public ReadOnly V(15) As Byte
    Public I As UShort
    Public Pc As UShort
    Public Sp As Byte
    Public ReadOnly Stack(15) As UShort

    Public DelayTimer As Byte
    Public SoundTimer As Byte

    Public ReadOnly Keys(15) As Boolean
    Public ReadOnly Display(DisplayWidthHighRes * DisplayHeightHighRes - 1) As Boolean
    Public ReadOnly RplFlags(15) As Byte

    Public DrawFlag As Boolean
    Public IsHighResMode As Boolean
    Public HasExited As Boolean
    
    ' Properties for dynamic display size
    Public ReadOnly Property DisplayWidth As Integer
        Get
            Return If(IsHighResMode, DisplayWidthHighRes, DisplayWidthLowRes)
        End Get
    End Property
    
    Public ReadOnly Property DisplayHeight As Integer
        Get
            Return If(IsHighResMode, DisplayHeightHighRes, DisplayHeightLowRes)
        End Get
    End Property

    Public Sub Reset()
        Array.Clear(Memory, 0, Memory.Length)
        Array.Clear(V, 0, V.Length)
        Array.Clear(Stack, 0, Stack.Length)
        Array.Clear(Keys, 0, Keys.Length)
        Array.Clear(Display, 0, Display.Length)
        Array.Clear(RplFlags, 0, RplFlags.Length)

        I = 0US
        Pc = ProgramStart
        Sp = 0
        DelayTimer = 0
        SoundTimer = 0
        DrawFlag = True
        IsHighResMode = False
        HasExited = False

        LoadFontSet()
        LoadExtendedFontSet()
    End Sub

    Public Sub LoadRom(path As String)
        Dim rom As Byte() = File.ReadAllBytes(path)
        If rom.Length + ProgramStart > Memory.Length Then
            Throw New InvalidOperationException("ROM too large for CHIP-8 memory.")
        End If

        Buffer.BlockCopy(rom, 0, Memory, ProgramStart, rom.Length)
        Pc = ProgramStart
    End Sub

    Public Sub SetKey(index As Integer, isDown As Boolean)
        If index < 0 OrElse index > 15 Then Return
        Keys(index) = isDown
    End Sub

    Public Sub TickTimers()
        If DelayTimer > 0 Then DelayTimer = CByte(DelayTimer - 1)
        If SoundTimer > 0 Then SoundTimer = CByte(SoundTimer - 1)
    End Sub

    Public Sub ExecuteCycle()
        ' Check if program has exited
        If HasExited Then
            Return
        End If
        
        ' Need two bytes: Pc and Pc+1 must both be within 0..4095.
        If Pc > &HFFEUS Then
            Throw New InvalidOperationException($"PC out of bounds: 0x{Pc:X4}")
        End If

        Dim opcode As UShort = CUShort((CUShort(Memory(Pc)) << 8) Or Memory(Pc + 1US))
        Pc = CUShort(Pc + 2US)

        Dim nnn As UShort = CUShort(opcode And &HFFFUS)
        Dim nn As Byte = CByte(opcode And &HFFUS)
        Dim n As Byte = CByte(opcode And &HFUS)

        Dim x As Integer = CInt((opcode And &HF00US) >> 8)
        Dim y As Integer = CInt((opcode And &HF0US) >> 4)

        Select Case opcode And &HF000US
            Case &H0US
                Select Case opcode
                    Case &HE0US ' CLS
                        Array.Clear(Display, 0, Display.Length)
                        DrawFlag = True

                    Case &HEEUS ' RET
                        If Sp = 0 Then Throw New InvalidOperationException("Stack underflow.")
                        Sp = CByte(Sp - 1)
                        Pc = Stack(Sp)
                    
                    Case &HFDUS ' 00FD: Exit (SCHIP)
                        ' Exit interpreter - stop execution
                        HasExited = True
                        
                    Case &HFEUS ' 00FE: Disable extended screen mode (SCHIP)
                        IsHighResMode = False
                        DrawFlag = True
                        
                    Case &HFFUS ' 00FF: Enable extended screen mode (SCHIP)
                        IsHighResMode = True
                        DrawFlag = True
                        
                    Case &HFBUS ' 00FB: Scroll right 4 pixels (SCHIP)
                        ScrollRight()
                        DrawFlag = True
                        
                    Case &HFCUS ' 00FC: Scroll left 4 pixels (SCHIP)
                        ScrollLeft()
                        DrawFlag = True

                    Case Else
                        ' Check for 00CN: Scroll down N pixels (SCHIP)
                        If (opcode And &HFF0US) = &HC0US Then
                            Dim scrollN As Integer = CInt(opcode And &HFUS)
                            ScrollDown(scrollN)
                            DrawFlag = True
                        End If
                        ' 0NNN ignored (RCA 1802 call), not used by most ROMs
                End Select

            Case &H1000US ' 1NNN: JP addr
                Pc = nnn

            Case &H2000US ' 2NNN: CALL addr
                If Sp >= 16 Then Throw New InvalidOperationException("Stack overflow.")
                Stack(Sp) = Pc
                Sp = CByte(Sp + 1)
                Pc = nnn

            Case &H3000US ' 3XNN: SE Vx, byte
                If V(x) = nn Then Pc = CUShort(Pc + 2US)

            Case &H4000US ' 4XNN: SNE Vx, byte
                If V(x) <> nn Then Pc = CUShort(Pc + 2US)

            Case &H5000US ' 5XY0: SE Vx, Vy
                If (opcode And &HFUS) = 0US Then
                    If V(x) = V(y) Then Pc = CUShort(Pc + 2US)
                End If

            Case &H6000US ' 6XNN: LD Vx, byte
                V(x) = nn

            Case &H7000US ' 7XNN: ADD Vx, byte
                V(x) = CByte((CInt(V(x)) + CInt(nn)) And &HFF)

            Case &H8000US
                Select Case opcode And &HFUS
                    Case &H0US ' 8XY0: LD Vx, Vy
                        V(x) = V(y)

                    Case &H1US ' 8XY1: OR
                        V(x) = CByte(V(x) Or V(y))

                    Case &H2US ' 8XY2: AND
                        V(x) = CByte(V(x) And V(y))

                    Case &H3US ' 8XY3: XOR
                        V(x) = CByte(V(x) Xor V(y))

                    Case &H4US ' 8XY4: ADD with carry
                        Dim sum As Integer = CInt(V(x)) + CInt(V(y))
                        V(15) = If(sum > 255, CByte(1), CByte(0))
                        V(x) = CByte(sum And &HFF)

                    Case &H5US ' 8XY5: SUB Vx -= Vy
                        V(15) = If(V(x) >= V(y), CByte(1), CByte(0))
                        V(x) = CByte((CInt(V(x)) - CInt(V(y))) And &HFF)

                    Case &H6US ' 8XY6: SHR Vx
                        V(15) = CByte(V(x) And 1)
                        V(x) = CByte(V(x) >> 1)

                    Case &H7US ' 8XY7: Vx = Vy - Vx
                        V(15) = If(V(y) >= V(x), CByte(1), CByte(0))
                        V(x) = CByte((CInt(V(y)) - CInt(V(x))) And &HFF)

                    Case &HEUS ' 8XYE: SHL Vx
                        V(15) = CByte((V(x) And &H80) >> 7)
                        V(x) = CByte((V(x) << 1) And &HFF)
                End Select

            Case &H9000US ' 9XY0: SNE Vx, Vy
                If (opcode And &HFUS) = 0US Then
                    If V(x) <> V(y) Then Pc = CUShort(Pc + 2US)
                End If

            Case &HA000US ' ANNN: LD I, addr
                I = nnn

            Case &HB000US ' BNNN: JP V0, addr
                Pc = CUShort(nnn + V(0))

            Case &HC000US ' CXNN: RND Vx, byte
                V(x) = CByte(_rng.Next(0, 256) And nn)

            Case &HD000US ' DXYN: DRW Vx, Vy, nibble
                ' Wrap starting coordinates to valid display range (0 to width-1, 0 to height-1)
                Dim px As Integer = V(x) Mod DisplayWidth
                Dim py As Integer = V(y) Mod DisplayHeight

                V(15) = 0
                
                ' SCHIP: If n=0, draw extended sprite (16x16 in high-res, 8x16 in low-res)
                If n = 0 Then
                    If IsHighResMode Then
                        ' Draw 16x16 sprite (SCHIP extended sprite in high-res mode)
                        For row As Integer = 0 To 15
                            Dim spriteByte1 As Byte = Memory(I + CUShort(row * 2))
                            Dim spriteByte2 As Byte = Memory(I + CUShort(row * 2 + 1))
                            
                            For col As Integer = 0 To 15
                                Dim bit As Boolean
                                If col < 8 Then
                                    bit = (spriteByte1 And (CByte(&H80 >> col))) <> 0
                                Else
                                    bit = (spriteByte2 And (CByte(&H80 >> (col - 8)))) <> 0
                                End If
                                
                                If bit Then
                                    Dim dx As Integer = px + col
                                    Dim dy As Integer = py + row
                                    
                                    ' Clip sprite pixels at screen boundaries
                                    If dx < DisplayWidth AndAlso dy < DisplayHeight Then
                                        Dim idx As Integer = dy * DisplayWidth + dx
                                        
                                        If Display(idx) Then V(15) = 1
                                        Display(idx) = Not Display(idx)
                                    End If
                                End If
                            Next
                        Next
                    Else
                        ' Draw 8x16 sprite (SCHIP in low-res mode)
                        For row As Integer = 0 To 15
                            Dim sprite As Byte = Memory(I + CUShort(row))
                            For col As Integer = 0 To 7
                                If (sprite And (CByte(&H80 >> col))) <> 0 Then
                                    Dim dx As Integer = px + col
                                    Dim dy As Integer = py + row
                                    
                                    ' Clip sprite pixels at screen boundaries
                                    If dx < DisplayWidth AndAlso dy < DisplayHeight Then
                                        Dim idx As Integer = dy * DisplayWidth + dx

                                        If Display(idx) Then V(15) = 1
                                        Display(idx) = Not Display(idx)
                                    End If
                                End If
                            Next
                        Next
                    End If
                Else
                    ' Standard 8xN sprite drawing
                    For row As Integer = 0 To n - 1
                        Dim sprite As Byte = Memory(I + CUShort(row))
                        For col As Integer = 0 To 7
                            If (sprite And (CByte(&H80 >> col))) <> 0 Then
                                Dim dx As Integer = px + col
                                Dim dy As Integer = py + row
                                
                                ' Clip sprite pixels at screen boundaries (classic CHIP-8 behavior)
                                ' Pixels outside the display are not drawn (no wrapping)
                                ' px and py are non-negative (wrapped above), so we only check upper bounds
                                If dx < DisplayWidth AndAlso dy < DisplayHeight Then
                                    Dim idx As Integer = dy * DisplayWidth + dx

                                    If Display(idx) Then V(15) = 1
                                    Display(idx) = Not Display(idx)
                                End If
                            End If
                        Next
                    Next
                End If

                DrawFlag = True

            Case &HE000US
                Select Case opcode And &HFFUS
                    Case &H9EUS ' EX9E: SKP Vx
                        If Keys(V(x) And &HF) Then Pc = CUShort(Pc + 2US)

                    Case &HA1US ' EXA1: SKNP Vx
                        If Not Keys(V(x) And &HF) Then Pc = CUShort(Pc + 2US)
                End Select

            Case &HF000US
                Select Case opcode And &HFFUS
                    Case &H7US ' FX07: LD Vx, DT
                        V(x) = DelayTimer

                    Case &HAUS ' FX0A: LD Vx, K (blocking)
                        Dim keyPressed As Integer = -1
                        For i As Integer = 0 To 15
                            If Keys(i) Then
                                keyPressed = i
                                Exit For
                            End If
                        Next

                        If keyPressed = -1 Then
                            Pc = CUShort(Pc - 2US) ' repeat this opcode
                        Else
                            V(x) = CByte(keyPressed)
                        End If

                    Case &H15US ' FX15: LD DT, Vx
                        DelayTimer = V(x)

                    Case &H18US ' FX18: LD ST, Vx
                        SoundTimer = V(x)

                    Case &H1EUS ' FX1E: ADD I, Vx
                        I = CUShort((I + V(x)) And &HFFFFUS)

                    Case &H29US ' FX29: LD F, Vx (font char)
                        I = CUShort((V(x) And &HF) * 5)
                        
                    Case &H30US ' FX30: LD HF, Vx (SCHIP extended font)
                        I = CUShort(&H50 + (V(x) And &HF) * 10)

                    Case &H33US ' FX33: BCD
                        Dim value As Integer = V(x)
                        Memory(I) = CByte(value \ 100)
                        Memory(I + 1US) = CByte((value \ 10) Mod 10)
                        Memory(I + 2US) = CByte(value Mod 10)

                    Case &H55US ' FX55: LD [I], V0..Vx
                        For j As Integer = 0 To x
                            Memory(I + CUShort(j)) = V(j)
                        Next

                    Case &H65US ' FX65: LD V0..Vx, [I]
                        For j As Integer = 0 To x
                            V(j) = Memory(I + CUShort(j))
                        Next
                        
                    Case &H75US ' FX75: Store V0..Vx in RPL user flags (SCHIP)
                        For j As Integer = 0 To Math.Min(x, 15)
                            RplFlags(j) = V(j)
                        Next
                        
                    Case &H85US ' FX85: Read V0..Vx from RPL user flags (SCHIP)
                        For j As Integer = 0 To Math.Min(x, 15)
                            V(j) = RplFlags(j)
                        Next
                End Select
        End Select
    End Sub

    Private Sub LoadFontSet()
        ' Font sprites (0-F), 5 bytes each, stored at 0x000.
        Dim font As Byte() = {
            &HF0, &H90, &H90, &H90, &HF0, ' 0
            &H20, &H60, &H20, &H20, &H70, ' 1
            &HF0, &H10, &HF0, &H80, &HF0, ' 2
            &HF0, &H10, &HF0, &H10, &HF0, ' 3
            &H90, &H90, &HF0, &H10, &H10, ' 4
            &HF0, &H80, &HF0, &H10, &HF0, ' 5
            &HF0, &H80, &HF0, &H90, &HF0, ' 6
            &HF0, &H10, &H20, &H40, &H40, ' 7
            &HF0, &H90, &HF0, &H90, &HF0, ' 8
            &HF0, &H90, &HF0, &H10, &HF0, ' 9
            &HF0, &H90, &HF0, &H90, &H90, ' A
            &HE0, &H90, &HE0, &H90, &HE0, ' B
            &HF0, &H80, &H80, &H80, &HF0, ' C
            &HE0, &H90, &H90, &H90, &HE0, ' D
            &HF0, &H80, &HF0, &H80, &HF0, ' E
            &HF0, &H80, &HF0, &H80, &H80  ' F
        }

        Buffer.BlockCopy(font, 0, Memory, 0, font.Length)
    End Sub
    
    Private Sub LoadExtendedFontSet()
        ' SCHIP extended font sprites (0-F), 10 bytes each, stored at 0x050.
        ' These are 8x10 pixel fonts for high-resolution mode
        Dim extendedFont As Byte() = {
            &HFF, &HFF, &HC3, &HC3, &HC3, &HC3, &HC3, &HC3, &HFF, &HFF, ' 0
            &H18, &H78, &H78, &H18, &H18, &H18, &H18, &H18, &HFF, &HFF, ' 1
            &HFF, &HFF, &H3, &H3, &HFF, &HFF, &HC0, &HC0, &HFF, &HFF,   ' 2
            &HFF, &HFF, &H3, &H3, &HFF, &HFF, &H3, &H3, &HFF, &HFF,     ' 3
            &HC3, &HC3, &HC3, &HC3, &HFF, &HFF, &H3, &H3, &H3, &H3,     ' 4
            &HFF, &HFF, &HC0, &HC0, &HFF, &HFF, &H3, &H3, &HFF, &HFF,   ' 5
            &HFF, &HFF, &HC0, &HC0, &HFF, &HFF, &HC3, &HC3, &HFF, &HFF, ' 6
            &HFF, &HFF, &H3, &H3, &H6, &HC, &H18, &H18, &H18, &H18,     ' 7
            &HFF, &HFF, &HC3, &HC3, &HFF, &HFF, &HC3, &HC3, &HFF, &HFF, ' 8
            &HFF, &HFF, &HC3, &HC3, &HFF, &HFF, &H3, &H3, &HFF, &HFF,   ' 9
            &H7E, &HFF, &HC3, &HC3, &HC3, &HFF, &HFF, &HC3, &HC3, &HC3, ' A
            &HFC, &HFC, &HC3, &HC3, &HFC, &HFC, &HC3, &HC3, &HFC, &HFC, ' B
            &H3F, &HFF, &HC0, &HC0, &HC0, &HC0, &HC0, &HC0, &HFF, &H3F, ' C
            &HFC, &HFC, &HC3, &HC3, &HC3, &HC3, &HC3, &HC3, &HFC, &HFC, ' D
            &HFF, &HFF, &HC0, &HC0, &HFF, &HFF, &HC0, &HC0, &HFF, &HFF, ' E
            &HFF, &HFF, &HC0, &HC0, &HFF, &HFF, &HC0, &HC0, &HC0, &HC0  ' F
        }

        Buffer.BlockCopy(extendedFont, 0, Memory, &H50, extendedFont.Length)
    End Sub
    
    Private Sub ScrollDown(n As Integer)
        ' Scroll display down by n pixels (SCHIP)
        Dim width As Integer = DisplayWidth
        Dim height As Integer = DisplayHeight
        
        ' Don't scroll if n is invalid or display is too small
        If n <= 0 OrElse n >= height Then Return
        
        ' Move pixels down from bottom to top
        For y As Integer = height - 1 To n Step -1
            For x As Integer = 0 To width - 1
                Dim srcIdx As Integer = (y - n) * width + x
                Dim dstIdx As Integer = y * width + x
                Display(dstIdx) = Display(srcIdx)
            Next
        Next
        
        ' Clear top n rows
        For y As Integer = 0 To n - 1
            For x As Integer = 0 To width - 1
                Display(y * width + x) = False
            Next
        Next
    End Sub
    
    Private Sub ScrollLeft()
        ' Scroll display left by 4 pixels (SCHIP)
        Dim width As Integer = DisplayWidth
        Dim height As Integer = DisplayHeight
        
        ' Don't scroll if display is too small
        If width < 4 Then Return
        
        For y As Integer = 0 To height - 1
            For x As Integer = 0 To width - 5
                Dim srcIdx As Integer = y * width + x + 4
                Dim dstIdx As Integer = y * width + x
                Display(dstIdx) = Display(srcIdx)
            Next
            ' Clear right 4 columns
            For x As Integer = width - 4 To width - 1
                Display(y * width + x) = False
            Next
        Next
    End Sub
    
    Private Sub ScrollRight()
        ' Scroll display right by 4 pixels (SCHIP)
        Dim width As Integer = DisplayWidth
        Dim height As Integer = DisplayHeight
        
        ' Don't scroll if display is too small
        If width < 4 Then Return
        
        For y As Integer = 0 To height - 1
            For x As Integer = width - 1 To 4 Step -1
                Dim srcIdx As Integer = y * width + x - 4
                Dim dstIdx As Integer = y * width + x
                Display(dstIdx) = Display(srcIdx)
            Next
            ' Clear left 4 columns
            For x As Integer = 0 To 3
                Display(y * width + x) = False
            Next
        Next
    End Sub
End Class