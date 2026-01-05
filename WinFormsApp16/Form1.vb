Option Strict On
Option Explicit On

Imports System.Drawing.Drawing2D
Imports System.IO
Imports System.Windows.Forms

Public Class Form1
    Private ReadOnly _chip8 As New Chip8()
    Private ReadOnly _frameTimer As New Timer()

    Private Const Scale As Integer = 10
    Private Const CyclesPerFrame As Integer = 12

    Private _isPaused As Boolean
    Private _lastRomPath As String
    Private _romLoaded As Boolean

    Public Sub New()
        InitializeComponent()

        DoubleBuffered = True
        KeyPreview = True

        Button1.Text = "Choose ROM..."

        _chip8.Reset()

        _frameTimer.Interval = 1000 \ 60
        AddHandler _frameTimer.Tick, AddressOf OnFrameTick
        _frameTimer.Start()
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

    Private Shared Function IsNesRom(filePath As String) As Boolean
        ' Check if the file is a NES ROM by looking for the iNES header
        ' NES ROMs start with "NES" followed by 0x1A (EOF character)
        Try
            If Not File.Exists(filePath) Then Return False
            
            Dim header(3) As Byte
            Using fs As New FileStream(filePath, FileMode.Open, FileAccess.Read, FileShare.Read)
                If fs.Length < 4 Then Return False
                fs.Read(header, 0, 4)
            End Using
            
            ' Check for iNES header: "NES" + 0x1A
            Return header(0) = &H4E AndAlso header(1) = &H45 AndAlso header(2) = &H53 AndAlso header(3) = &H1A
        Catch
            Return False
        End Try
    End Function

    Private Sub SetChip8KeyState(keyCode As Keys, isDown As Boolean)
        Dim chip8Key As Integer
        If TryMapToChip8Key(keyCode, chip8Key) Then
            _chip8.SetKey(chip8Key, isDown)
        End If
    End Sub

    Protected Overrides Function ProcessCmdKey(ByRef msg As Message, keyData As Keys) As Boolean
        Dim keyCode As Keys = keyData And Keys.KeyCode

        Dim chip8Key As Integer
        If TryMapToChip8Key(keyCode, chip8Key) Then
            _chip8.SetKey(chip8Key, True)
            Return True
        End If

        Return MyBase.ProcessCmdKey(msg, keyData)
    End Function

    Protected Overrides Sub OnKeyDown(e As KeyEventArgs)
        MyBase.OnKeyDown(e)

        Dim chip8Key As Integer
        If TryMapToChip8Key(e.KeyCode, chip8Key) Then
            _chip8.SetKey(chip8Key, True)
            e.Handled = True
        End If
    End Sub

    Protected Overrides Sub OnKeyUp(e As KeyEventArgs)
        MyBase.OnKeyUp(e)

        Dim chip8Key As Integer
        If TryMapToChip8Key(e.KeyCode, chip8Key) Then
            _chip8.SetKey(chip8Key, False)
            e.Handled = True
        End If
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Using ofd As New OpenFileDialog()
            ofd.Title = "Open ROM"
            ofd.Filter = "All ROMs|*.ch8;*.c8;*.rom;*.nes|CHIP-8 ROMs|*.ch8;*.c8;*.rom|NES ROMs|*.nes|All Files|*.*"
            ofd.InitialDirectory = If(String.IsNullOrWhiteSpace(_lastRomPath), Application.StartupPath, Path.GetDirectoryName(_lastRomPath))

            If ofd.ShowDialog(Me) <> DialogResult.OK Then Return

            _lastRomPath = ofd.FileName
            
            ' Check if this is a NES ROM
            If IsNesRom(_lastRomPath) Then
                MessageBox.Show(Me, "NES ROM detected!" & Environment.NewLine & Environment.NewLine & 
                               "This is a Nintendo Entertainment System ROM file. " &
                               "NES emulation is not yet supported in this application." & Environment.NewLine & Environment.NewLine &
                               "This application currently only supports CHIP-8 ROMs (.ch8, .c8, .rom).",
                               "NES ROM Not Supported",
                               MessageBoxButtons.OK,
                               MessageBoxIcon.Information)
                Return
            End If
            
            _romLoaded = True
            LoadAndStartRom(_lastRomPath)
        End Using
    End Sub

    Private Sub ButtonReset_Click(sender As Object, e As EventArgs) Handles ButtonReset.Click
        If String.IsNullOrWhiteSpace(_lastRomPath) OrElse Not File.Exists(_lastRomPath) Then
            _chip8.Reset()
            _chip8.DrawFlag = True
            Invalidate()
            Return
        End If

        LoadAndStartRom(_lastRomPath)
    End Sub

    Private Sub ButtonPause_Click(sender As Object, e As EventArgs) Handles ButtonPause.Click
        _isPaused = Not _isPaused
        ButtonPause.Text = If(_isPaused, "Resume", "Pause")
    End Sub

    Private Sub LoadAndStartRom(inPath As String)
        _chip8.Reset()
        _chip8.LoadRom(inPath)

        Text = $"CHIP-8 - {Path.GetFileName(inPath)}"
        _isPaused = False
        ButtonPause.Text = "Pause"

        _chip8.DrawFlag = True
        Invalidate()
    End Sub

    Private Sub OnFrameTick(sender As Object, e As EventArgs)
        If Not _romLoaded OrElse _isPaused Then Return

        For i As Integer = 0 To CyclesPerFrame - 1
            _chip8.ExecuteCycle()
        Next

        _chip8.TickTimers()

        If _chip8.DrawFlag Then
            _chip8.DrawFlag = False
            Invalidate()
        End If
    End Sub

    Protected Overrides Sub OnPaint(e As PaintEventArgs)
        MyBase.OnPaint(e)

        e.Graphics.InterpolationMode = InterpolationMode.NearestNeighbor
        e.Graphics.PixelOffsetMode = PixelOffsetMode.Half
        e.Graphics.Clear(Color.Black)

        For y As Integer = 0 To Chip8.DisplayHeight - 1
            For x As Integer = 0 To Chip8.DisplayWidth - 1
                Dim idx As Integer = (y * Chip8.DisplayWidth) + x
                If _chip8.Display(idx) Then
                    e.Graphics.FillRectangle(Brushes.White, x * Scale, y * Scale, Scale, Scale)
                End If
            Next
        Next
    End Sub

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles Me.Load
        Me.KeyPreview = True
    End Sub
End Class
