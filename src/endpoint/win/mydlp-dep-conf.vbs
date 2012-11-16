MYDLP_SERVER="10.0.0.2"
MYDLP_SHARE="\\mydlp\downloads"

Const HKEY_LOCAL_MACHINE = &H80000002
strComputer = "."

Set objRegistry=GetObject("winmgmts:{impersonationLevel=impersonate}!\\" &_ 
strComputer & "\root\default:StdRegProv")

On Error Resume Next
 
Set oShell = CreateObject("Wscript.Shell")
 
'Determine if 64 bit system

strKeyPath = "SYSTEM\CurrentControlSet\Control\Session Manager\Environment"
strValueName = "PROCESSOR_ARCHITECTURE"
objRegistry.GetStringValue HKEY_LOCAL_MACHINE,strKeyPath,strValueName,strValue

If strValue = "AMD64" Then
	strKeyPath = "SOFTWARE\Wow6432Node\"
Else 
	strKeyPath = "SOFTWARE\"
End If

strValueName = "management_server"
strValue = MYDLP_SERVER


If oReg.EnumKey("MyDLP", strKeyPath, arrSubKeys) <> 0 Then
	objRegistry.CreateKey HKEY_LOCAL_MACHINE,strKeyPath & "\MyDLP"
End If


objRegistry.SetStringValue HKEY_LOCAL_MACHINE,strKeyPath & "\MyDLP",strValueName,strValue

Set objWMIService = GetObject("winmgmts:" _
		& "{impersonationLevel=impersonate}!\\" & strComputer & "\root\cimv2")

Set colOperatingSystems = objWMIService.ExecQuery _
	("Select * from Win32_OperatingSystem")

For Each objOperatingSystem in colOperatingSystems    
	a = Split(objOperatingSystem.Caption)
	If a(1) <> "Windows" Or a(2) <> "XP" Then
'Quit on non XP machines
		WScript.Quit(0)
	End If
Next

'VC++ Redist 2005 8.0.59193 key HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\{837b34e3-7c30-493c-8f6a-2b0f04e2912c}

'VC++ Redist 2008 9.0.21022 key HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\{FF66E9F6-83E7-3A3E-AF14-8DE9A809A6A4}

'MS .NET 3.5 SP1 key HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\{CE2CDD62-0124-36CA-84D3-9F4DCF5C5BD9}



strValueName = "DisplayName"

strKeyPath = "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\{837b34e3-7c30-493c-8f6a-2b0f04e2912c}"
objRegistry.GetStringValue HKEY_LOCAL_MACHINE,strKeyPath,strValueName,strValue

If IsNull(strValue) Then 
    oShell.Run MYDLP_SHARE & "\deps\win-xp\vcredist_x86_2005.exe /q:a /c:""msiexec /i vcredist.msi /qn /l*v %temp%\vcredist_x86.log""", 1, True
End If


strKeyPath = "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\{FF66E9F6-83E7-3A3E-AF14-8DE9A809A6A4}"
objRegistry.GetStringValue HKEY_LOCAL_MACHINE,strKeyPath,strValueName,strValue

If IsNull(strValue) Then 
    oShell.Run MYDLP_SHARE & "\deps\win-xp\vcredist_x86_2008.exe /q", 1, True
End If


strKeyPath = "SOFTWARE\Microsoft\NET Framework Setup\NDP\v3.5"
strValueName = "Install"
objRegistry.GetDWORDValue HKEY_LOCAL_MACHINE,strKeyPath,strValueName,dwValue

If IsNull(dwValue) Or dwValue <> 1 Then
    oShell.Run MYDLP_SHARE &  "\deps\win-xp\dotnetfx35.exe /q /norestart", 1, True
End If

oShell.Run "sc stop mydlpepwatchdog", 1, True
oShell.Run "sc stop mydlpepwin", 1, True
oShell.Run "sc start mydlpepwin", 1, True

