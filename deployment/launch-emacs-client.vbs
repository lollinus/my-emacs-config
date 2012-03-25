'*******************************************************************************
'  Start emacs if it's not running & open file if script called with argument
'*******************************************************************************
For Each process In GetObject("winmgmts:\\.\root\cimv2"). _
                    ExecQuery("Select * From Win32_Process")
    If process.name = "emacs.exe" Then
        ' connect to emacs & open file or make emacs the top window
        ' replace -f arg with your server file location
        run path("emacsclientw.exe") & " -f %HOME%\.emacs.d\server\server -n " & _
            fileOr("-e (raise-frame)")
        WScript.Quit
    End If
Next
run path("runemacs.exe") & " " & fileOr(Null)  ' start new emacs

Sub run(cmd)  ' run the cmd
    WScript.CreateObject("WScript.Shell").Run(cmd)
End Sub

Function path(exe)  ' return full, quoted path to executable
    scriptDir = CreateObject("Scripting.FileSystemObject"). _
                GetParentFolderName(WScript.ScriptFullName)
    path = """" & scriptDir & "\" & exe & """"
End Function

Function fileOr(default)  ' return script arg (filename) or default
    If WScript.Arguments.Count = 1 Then
        fileOr =  """" & WScript.Arguments(0) & """"
    Else
        fileOr = default
    End If
End Function
