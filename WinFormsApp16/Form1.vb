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

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Using ofd As New OpenFileDialog()
            ofd.Title = "Open CHIP-8 ROM"
            ofd.Filter = "CHIP-8 ROMs|*.ch8;*.c8;*.rom;*.*"
            ofd.InitialDirectory = If(String.IsNullOrWhiteSpace(_lastRomPath), Application.StartupPath, Path.GetDirectoryName(_lastRomPath))

            If ofd.ShowDialog(Me) <> DialogResult.OK Then Return

            _lastRomPath = ofd.FileName
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

    Protected Overrides Function ProcessCmdKey(ByRef msg As Message, keyData As Keys) As Boolean
        Dim keyCode As Keys = keyData And Keys.KeyCode

        Select Case keyCode
            Case Keys.Up
                _chip8.SetKey(&HC, True)
                Return True

            Case Keys.Down
                _chip8.SetKey(&HD, True)
                Return True
        End Select

        Return MyBase.ProcessCmdKey(msg, keyData)
    End Function

    Protected Overrides Sub OnKeyUp(e As KeyEventArgs)
        MyBase.OnKeyUp(e)

        Select Case e.KeyCode
            Case Keys.Up
                _chip8.SetKey(&HC, False)
                e.Handled = True
            Case Keys.Down
                _chip8.SetKey(&HD, False)
                e.Handled = True
        End Select
    End Sub

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles Me.Load
        Me.KeyPreview = True
    End Sub
End Class
