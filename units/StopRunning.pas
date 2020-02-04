Unit StopRunning;
Interface
Implementation
Uses SysUtils;
Initialization
Finalization
	If FileExists('.running') Then
		DeleteFile('.running');
End.