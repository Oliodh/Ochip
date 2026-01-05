<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class Form1
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()>
    Protected Overrides Sub Dispose(disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
            If disposing Then
                _nesBitmap?.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    Private components As System.ComponentModel.IContainer

    <System.Diagnostics.DebuggerStepThrough()>
    Private Sub InitializeComponent()
        ButtonReset = New Button()
        ButtonPause = New Button()
        Button1 = New Button()
        SuspendLayout()
        ' 
        ' ButtonReset
        ' 
        ButtonReset.Anchor = AnchorStyles.Bottom Or AnchorStyles.Left
        ButtonReset.Location = New Point(138, 279)
        ButtonReset.Name = "ButtonReset"
        ButtonReset.Size = New Size(120, 29)
        ButtonReset.TabIndex = 1
        ButtonReset.Text = "Reset"
        ButtonReset.UseVisualStyleBackColor = True
        ' 
        ' ButtonPause
        ' 
        ButtonPause.Anchor = AnchorStyles.Bottom Or AnchorStyles.Left
        ButtonPause.Location = New Point(264, 279)
        ButtonPause.Name = "ButtonPause"
        ButtonPause.Size = New Size(120, 29)
        ButtonPause.TabIndex = 2
        ButtonPause.Text = "Pause"
        ButtonPause.UseVisualStyleBackColor = True
        ' 
        ' Button1
        ' 
        Button1.Location = New Point(40, 278)
        Button1.Name = "Button1"
        Button1.Size = New Size(93, 31)
        Button1.TabIndex = 3
        Button1.Text = "Button1"
        Button1.UseVisualStyleBackColor = True
        ' 
        ' Form1
        ' 
        AutoScaleDimensions = New SizeF(8.0F, 20.0F)
        AutoScaleMode = AutoScaleMode.Font
        ClientSize = New Size(640, 320)
        Controls.Add(Button1)
        Controls.Add(ButtonPause)
        Controls.Add(ButtonReset)
        Name = "Form1"
        Text = "CHIP-8"
        ResumeLayout(False)
    End Sub
    Friend WithEvents ButtonReset As Button
    Friend WithEvents ButtonPause As Button
    Friend WithEvents Button1 As Button

End Class
