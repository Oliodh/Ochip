Option Strict On
Option Explicit On

Imports System.Drawing.Drawing2D
Imports System.IO
Imports System.Windows.Forms

Public Class Form1
    Private ReadOnly _chip8 As New Chip8()
    Private ReadOnly _nes As New NES()
    Private ReadOnly _frameTimer As New Timer()
    Private _nesBitmap As Bitmap
    Private _chip8Bitmap As Bitmap
    Private _chip8PixelBuffer() As Integer

    Private Const Chip8ScaleLowRes As Integer = 10
    Private Const Chip8ScaleHighRes As Integer = 5
    Private Const CyclesPerFrame As Integer = 12
    Private Const PixelColorWhite As Integer = &HFFFFFFFF ' ARGB white
    Private Const PixelColorBlack As Integer = &HFF000000 ' ARGB black
    
    Private _lastChip8HighResMode As Boolean = False

    Private _isPaused As Boolean
    Private _lastRomPath As String
    Private _romLoaded As Boolean
    Private _currentEmulator As EmulatorType = EmulatorType.Chip8

    Private Enum EmulatorType
        Chip8
        NES
    End Enum

    Public Sub New()
        InitializeComponent()

        DoubleBuffered = True
        KeyPreview = True

        Button1.Text = "Choose ROM..."

        _chip8.Reset()

        _frameTimer.Interval = 1000 \ 60
        AddHandler _frameTimer.Tick, AddressOf OnFrameTick
        _frameTimer.Start()
        
        ' Handle form closing to clean up resources
        AddHandler Me.FormClosing, AddressOf Form1_FormClosing
    End Sub
    
    Private Sub Form1_FormClosing(sender As Object, e As System.Windows.Forms.FormClosingEventArgs)
        _nesBitmap?.Dispose()
        _chip8Bitmap?.Dispose()
    End Sub

    Private Shared Function TryMapToChip8Key(keyCode As Keys, ByRef chip8Key As Integer) As Boolean
        ' Standard CHIP-8 keyboard layout:
        ' 1 2 3 C
        ' 4 5 6 D
        ' 7 8 9 E
        ' A 0 B F
        '
        ' Typical PC mapping:
        ' 1 2 3 4
        ' Q W E R
        ' A S D F
        ' Z X C V

        Select Case keyCode
            Case Keys.D1 : chip8Key = &H1 : Return True
            Case Keys.D2 : chip8Key = &H2 : Return True
            Case Keys.D3 : chip8Key = &H3 : Return True
            Case Keys.D4 : chip8Key = &HC : Return True

            Case Keys.Q : chip8Key = &H4 : Return True
            Case Keys.W : chip8Key = &H5 : Return True
            Case Keys.E : chip8Key = &H6 : Return True
            Case Keys.R : chip8Key = &HD : Return True

            Case Keys.A : chip8Key = &H7 : Return True
            Case Keys.S : chip8Key = &H8 : Return True
            Case Keys.D : chip8Key = &H9 : Return True
            Case Keys.F : chip8Key = &HE : Return True

            Case Keys.Z : chip8Key = &HA : Return True
            Case Keys.X : chip8Key = &H0 : Return True
            Case Keys.C : chip8Key = &HB : Return True
            Case Keys.V : chip8Key = &HF : Return True

            ' Extra convenience: arrow keys often expected by games like Pong.
            ' Right paddle: Up/Down -> C/D (common convention)
            Case Keys.Up : chip8Key = &HC : Return True
            Case Keys.Down : chip8Key = &HD : Return True

            ' Extra convenience: left paddle with Left/Right -> 1/4 (common convention)
            Case Keys.Left : chip8Key = &H1 : Return True
            Case Keys.Right : chip8Key = &H4 : Return True
        End Select

        chip8Key = -1
        Return False
    End Function

    Private Shared Function TryMapToNESButton(keyCode As Keys, ByRef nesButton As Integer) As Boolean
        ' NES Controller layout:
        ' 0 = A
        ' 1 = B
        ' 2 = Select
        ' 3 = Start
        ' 4 = Up
        ' 5 = Down
        ' 6 = Left
        ' 7 = Right

        Select Case keyCode
            Case Keys.X : nesButton = 0 : Return True     ' A
            Case Keys.Z : nesButton = 1 : Return True     ' B
            Case Keys.Back : nesButton = 2 : Return True  ' Select
            Case Keys.Enter : nesButton = 3 : Return True ' Start
            Case Keys.Up : nesButton = 4 : Return True
            Case Keys.Down : nesButton = 5 : Return True
            Case Keys.Left : nesButton = 6 : Return True
            Case Keys.Right : nesButton = 7 : Return True
        End Select

        nesButton = -1
        Return False
    End Function

    Private Sub SetChip8KeyState(keyCode As Keys, isDown As Boolean)
        Dim chip8Key As Integer
        If TryMapToChip8Key(keyCode, chip8Key) Then
            _chip8.SetKey(chip8Key, isDown)
        End If
    End Sub

    Protected Overrides Function ProcessCmdKey(ByRef msg As Message, keyData As Keys) As Boolean
        Dim keyCode As Keys = keyData And Keys.KeyCode

        If _currentEmulator = EmulatorType.Chip8 Then
            Dim chip8Key As Integer
            If TryMapToChip8Key(keyCode, chip8Key) Then
                _chip8.SetKey(chip8Key, True)
                Return True
            End If
        Else
            Dim nesButton As Integer
            If TryMapToNESButton(keyCode, nesButton) Then
                _nes.SetController1Button(nesButton, True)
                Return True
            End If
        End If

        Return MyBase.ProcessCmdKey(msg, keyData)
    End Function

    Protected Overrides Sub OnKeyDown(e As KeyEventArgs)
        MyBase.OnKeyDown(e)

        If _currentEmulator = EmulatorType.Chip8 Then
            Dim chip8Key As Integer
            If TryMapToChip8Key(e.KeyCode, chip8Key) Then
                _chip8.SetKey(chip8Key, True)
                e.Handled = True
            End If
        Else
            Dim nesButton As Integer
            If TryMapToNESButton(e.KeyCode, nesButton) Then
                _nes.SetController1Button(nesButton, True)
                e.Handled = True
            End If
        End If
    End Sub

    Protected Overrides Sub OnKeyUp(e As KeyEventArgs)
        MyBase.OnKeyUp(e)

        If _currentEmulator = EmulatorType.Chip8 Then
            Dim chip8Key As Integer
            If TryMapToChip8Key(e.KeyCode, chip8Key) Then
                _chip8.SetKey(chip8Key, False)
                e.Handled = True
            End If
        Else
            Dim nesButton As Integer
            If TryMapToNESButton(e.KeyCode, nesButton) Then
                _nes.SetController1Button(nesButton, False)
                e.Handled = True
            End If
        End If
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Using ofd As New OpenFileDialog()
            ofd.Title = "Open ROM"
            ofd.Filter = "All ROMs|*.ch8;*.c8;*.rom;*.nes|CHIP-8 ROMs|*.ch8;*.c8;*.rom|NES ROMs|*.nes|All Files|*.*"
            ofd.InitialDirectory = If(String.IsNullOrWhiteSpace(_lastRomPath), Application.StartupPath, Path.GetDirectoryName(_lastRomPath))

            If ofd.ShowDialog(Me) <> DialogResult.OK Then Return

            _lastRomPath = ofd.FileName
            _romLoaded = False

            ' Detect emulator type by file extension
            Dim ext As String = Path.GetExtension(_lastRomPath).ToLowerInvariant()
            If ext = ".nes" Then
                _currentEmulator = EmulatorType.NES
            Else
                _currentEmulator = EmulatorType.Chip8
            End If

            LoadAndStartRom(_lastRomPath)
        End Using
    End Sub

    Private Sub ButtonReset_Click(sender As Object, e As EventArgs) Handles ButtonReset.Click
        If String.IsNullOrWhiteSpace(_lastRomPath) OrElse Not File.Exists(_lastRomPath) Then
            If _currentEmulator = EmulatorType.Chip8 Then
                _chip8.Reset()
                _chip8.DrawFlag = True
            Else
                _nes.Reset()
                _nes.DrawFlag = True
            End If
            Invalidate()
            Return
        End If

        LoadAndStartRom(_lastRomPath)
    End Sub

    Private Sub ButtonPause_Click(sender As Object, e As EventArgs) Handles ButtonPause.Click
        _isPaused = Not _isPaused
        ButtonPause.Text = If(_isPaused, "Resume", "Pause")
    End Sub
    
    Private Sub EnsureChip8BitmapSize(width As Integer, height As Integer)
        ' Recreate bitmap if size doesn't match
        If _chip8Bitmap Is Nothing OrElse _chip8Bitmap.Width <> width OrElse _chip8Bitmap.Height <> height Then
            _chip8Bitmap?.Dispose()
            _chip8Bitmap = New Bitmap(width, height, System.Drawing.Imaging.PixelFormat.Format32bppArgb)
        End If
        
        ' Recreate pixel buffer if size doesn't match
        Dim pixelCount As Integer = width * height
        If _chip8PixelBuffer Is Nothing OrElse _chip8PixelBuffer.Length <> pixelCount Then
            ReDim _chip8PixelBuffer(pixelCount - 1)
        End If
    End Sub

    Private Sub LoadAndStartRom(inPath As String)
        Try
            If _currentEmulator = EmulatorType.Chip8 Then
                _chip8.Reset()
                _chip8.LoadRom(inPath)
                _chip8.DrawFlag = True
                Text = $"CHIP-8 Emulator - {Path.GetFileName(inPath)}"
                ' Resize window for CHIP-8 (start with low-res, will adjust dynamically)
                ClientSize = New Size(Chip8.DisplayWidthLowRes * Chip8ScaleLowRes, Chip8.DisplayHeightLowRes * Chip8ScaleLowRes)
                _lastChip8HighResMode = False
                ' Create persistent bitmap and pixel buffer for CHIP-8 rendering
                EnsureChip8BitmapSize(Chip8.DisplayWidthLowRes, Chip8.DisplayHeightLowRes)
                ' Dispose NES bitmap if switching from NES
                _nesBitmap?.Dispose()
            Else
                _nes.Reset()
                _nes.LoadRom(inPath)
                _nes.DrawFlag = True
                Text = $"NES Emulator - {Path.GetFileName(inPath)}"
                ' Resize window for NES
                ClientSize = New Size(NES.DisplayWidth, NES.DisplayHeight)
                ' Create persistent bitmap for NES rendering
                _nesBitmap?.Dispose()
                _nesBitmap = New Bitmap(NES.DisplayWidth, NES.DisplayHeight, System.Drawing.Imaging.PixelFormat.Format32bppArgb)
                ' Dispose CHIP-8 bitmap if switching from CHIP-8
                _chip8Bitmap?.Dispose()
            End If

            _romLoaded = True
            _isPaused = False
            ButtonPause.Text = "Pause"
            Invalidate()
        Catch ex As Exception
            _romLoaded = False
            MessageBox.Show(Me, "Failed to load ROM. Please confirm the file is a valid CHIP-8 or NES ROM.", "ROM Load Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Try
    End Sub

    Private Sub OnFrameTick(sender As Object, e As EventArgs)
        If Not _romLoaded OrElse _isPaused Then Return

        If _currentEmulator = EmulatorType.Chip8 Then
            For i As Integer = 0 To CyclesPerFrame - 1
                _chip8.ExecuteCycle()
            Next

            _chip8.TickTimers()
            
            ' Check if display mode changed (SCHIP mode switching)
            If _chip8.IsHighResMode <> _lastChip8HighResMode Then
                _lastChip8HighResMode = _chip8.IsHighResMode
                Dim scale As Integer = If(_chip8.IsHighResMode, Chip8ScaleHighRes, Chip8ScaleLowRes)
                ClientSize = New Size(_chip8.DisplayWidth * scale, _chip8.DisplayHeight * scale)
                ' Recreate bitmap and pixel buffer with new resolution
                EnsureChip8BitmapSize(_chip8.DisplayWidth, _chip8.DisplayHeight)
            End If

            If _chip8.DrawFlag Then
                _chip8.DrawFlag = False
                Invalidate()
            End If
        Else
            _nes.ExecuteFrame()

            If _nes.DrawFlag Then
                _nes.DrawFlag = False
                Invalidate()
            End If
        End If
    End Sub

    Protected Overrides Sub OnPaint(e As PaintEventArgs)
        MyBase.OnPaint(e)

        e.Graphics.Clear(Color.Black)

        If _currentEmulator = EmulatorType.Chip8 Then
            ' Render CHIP-8 display using persistent bitmap
            Dim width As Integer = _chip8.DisplayWidth
            Dim height As Integer = _chip8.DisplayHeight
            
            ' Ensure bitmap and pixel buffer match current display size
            EnsureChip8BitmapSize(width, height)
            
            If _chip8Bitmap IsNot Nothing AndAlso _chip8PixelBuffer IsNot Nothing Then
                Dim pixelCount As Integer = width * height
                
                ' Lock the bitmap for fast pixel manipulation
                Dim bmpData As System.Drawing.Imaging.BitmapData = _chip8Bitmap.LockBits(
                    New Rectangle(0, 0, _chip8Bitmap.Width, _chip8Bitmap.Height),
                    System.Drawing.Imaging.ImageLockMode.WriteOnly,
                    System.Drawing.Imaging.PixelFormat.Format32bppArgb)
                
                Try
                    ' Convert CHIP-8 display to ARGB pixels using persistent buffer
                    For i As Integer = 0 To pixelCount - 1
                        _chip8PixelBuffer(i) = If(_chip8.Display(i), PixelColorWhite, PixelColorBlack)
                    Next
                    
                    ' Copy pixels to bitmap
                    System.Runtime.InteropServices.Marshal.Copy(_chip8PixelBuffer, 0, bmpData.Scan0, pixelCount)
                Finally
                    ' Always unlock the bitmap, even if an exception occurs
                    _chip8Bitmap.UnlockBits(bmpData)
                End Try
                
                ' Draw the bitmap scaled to the client area
                e.Graphics.InterpolationMode = InterpolationMode.NearestNeighbor
                e.Graphics.PixelOffsetMode = PixelOffsetMode.Half
                e.Graphics.DrawImage(_chip8Bitmap, 0, 0, ClientSize.Width, ClientSize.Height)
            End If
        Else
            ' Render NES display using persistent bitmap
            If _nesBitmap IsNot Nothing Then
                Dim bmpData As System.Drawing.Imaging.BitmapData = _nesBitmap.LockBits(
                    New Rectangle(0, 0, _nesBitmap.Width, _nesBitmap.Height),
                    System.Drawing.Imaging.ImageLockMode.WriteOnly,
                    System.Drawing.Imaging.PixelFormat.Format32bppArgb)

                Try
                    System.Runtime.InteropServices.Marshal.Copy(_nes.DisplayBuffer, 0, bmpData.Scan0, _nes.DisplayBuffer.Length)
                Finally
                    ' Always unlock the bitmap, even if an exception occurs
                    _nesBitmap.UnlockBits(bmpData)
                End Try

                e.Graphics.InterpolationMode = InterpolationMode.NearestNeighbor
                e.Graphics.DrawImage(_nesBitmap, 0, 0, ClientSize.Width, ClientSize.Height)
            End If
        End If
    End Sub

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles Me.Load
        Me.KeyPreview = True
    End Sub
End Class
