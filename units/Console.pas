{
	This unit can be a replacement to build-in unit crt.
	For more details, please refer to the GitHub repository's README file.
	https://github.com/AnsonYeung/PascalCrt
}
{$IFNDEF Windows}
	{$Error Console unit is for Windows only, for other platforms, please use the native crt unit}
{$ENDIF}
{$mode objfpc}
{$inline on}
{$calling stdcall}
Unit Console;
Interface
Uses Math, SysUtils;
Const
Black: Integer = 0;
Blue: Integer = 1;
Green: Integer = 2;
Red: Integer = 4;
Gray: Integer = 8;
Aqua: Integer = 3;
Purple: Integer = 5;
Yellow: Integer = 6;
LightGray: Integer = 7;
LightBlue: Integer = 9;
LightGreen: Integer = 10;
LightRed: Integer = 12;
LightAqua: Integer = 11;
LightPurple: Integer = 13;
LightYellow: Integer = 14;
White: Integer = 15;

v : Char = #186;
h : Char = #205;
cul : Char = #201;
cur : Char = #187;
cll : Char = #200;
clr : Char = #188;

LF_FACESIZE = 32;
STD_INPUT_HANDLE: DWord = DWord(-10);
STD_OUTPUT_HANDLE: DWord = DWord(-11);
STD_ERROR_HANDLE: DWord = DWord(-12);
ENABLE_LINE_INPUT: DWord = 2;
ENABLE_ECHO_INPUT: DWord = 4;
ENABLE_PROCESSED_INPUT: DWord = 1;
ENABLE_WINDOW_INPUT: DWord = 8;
ENABLE_MOUSE_INPUT: DWord = 16;
ENABLE_PROCESSED_OUTPUT: DWord = 1;
ENABLE_WRAP_AT_EOL_OUTPUT: DWord = 2;
ENABLE_INSERT_MODE: DWord = $0020;
ENABLE_QUICK_EDIT_MODE: DWord = $0040;
ENABLE_EXTENDED_FLAGS: DWord = $0080;
ENABLE_AUTO_POSITION: DWord = $0100;
ENABLE_VIRTUAL_TERMINAL_INPUT: DWord = $0200;
ENABLE_VIRTUAL_TERMINAL_PROCESSING: DWord = $0004;
DISABLE_NEWLINE_AUTO_RETURN: DWord = $0008;
ENABLE_LVB_GRID_WORLDWIDE: DWord = $0010;

VK_UP = $26;
VK_DOWN = $28;
VK_LEFT = $25;
VK_RIGHT = $27;
VK_RETURN = $0D;
MOUSE_MOVED = $01;

Type
Handle = System.THandle;
FACETYPE = Array[0..(LF_FACESIZE)-1] Of WChar;
Coord = Record
	X : SmallInt;
	Y : SmallInt;
End;

KEY_EVENT_RECORD = Packed Record
	bKeyDown : LongBool;
	wRepeatCount : Word;
	wVirtualKeyCode : Word;
	wVirtualScanCode : Word;
	Case LongInt Of
		0 : ( UnicodeChar : WChar;
		dwControlKeyState : DWord; );
		1 : ( AsciiChar : Char );
End;

MOUSE_EVENT_RECORD = Record
	dwMousePosition : Coord;
	dwButtonState : DWord;
	dwControlKeyState : DWord;
	dwEventFlags : DWord;
End;

WINDOW_BUFFER_SIZE_RECORD = Record
	dwSize : Coord;
End;

MENU_EVENT_RECORD = Record
	dwCommandId : Cardinal;
End;

FOCUS_EVENT_RECORD = Record
	bSetFocus : LongBool;
End;

INPUT_RECORD = Record
	EventType: Word;
	Reserved: Word;
	Event : Record Case LongInt Of
		0 : ( KeyEvent : KEY_EVENT_RECORD );
		1 : ( MouseEvent : MOUSE_EVENT_RECORD );
		2 : ( WindowBufferSizeEvent : WINDOW_BUFFER_SIZE_RECORD );
		3 : ( MenuEvent : MENU_EVENT_RECORD );
		4 : ( FocusEvent : FOCUS_EVENT_RECORD );
	End;
End;

CHAR_INFO = Record
	Case LongInt Of
		0 : ( UnicodeChar : WChar;
		Attributes : Word);
		1 : ( AsciiChar : Char );
End;
PCHAR_INFO = ^CHAR_INFO;

Procedure SetConsoleTitle(Const title: AnsiString);
Procedure SetConsoleFont(Const FaceName: FACETYPE; Const x, y: Integer);
Procedure SetConsoleSize(Const Width: Integer; Const Height: Integer);
Procedure SetConsoleBuffer(Const Width: Integer; Const Height: Integer);
Procedure PollConsoleInput(Var irInBuf: Array Of INPUT_RECORD; Const bufSize: DWord; Var cNumRead: DWord);
Procedure PeekConsoleInput(Var irInBuf: Array Of INPUT_RECORD; Const bufSize: DWord; Var cNumRead: DWord);
Procedure ClrScr();
Procedure SetConsoleColor(Const color: Word);
Procedure TextBackground(Const color: Integer);
Procedure TextColor(Const color: Integer);
Procedure GoToXY(Const x, y: Integer);
Procedure CursorOff();
Procedure CursorOn();
Procedure WriteAttr(Const attrs: Array Of Word; Const x, y: Integer);
Function ReadKey(): Word;
Procedure FlushInput();
Function CreateBuffer(): Handle;
Procedure SetActiveBuffer(ob: Handle);
Function GetActiveBuffer(): Handle;
// Added for this program
Procedure WriteDupAttr(Const X: Integer; Const Y: Integer; Const n: Integer);
Procedure ReadEnter();
Procedure ReadInt(Var i: Integer);
Procedure OutputFastBuffer(Const fastBuffer: PCHAR_INFO; Const X, Y, Width, Height: Integer);
Procedure DisableClose();
// original internal function
// public for this program
Function StrDup(Const str: String; Const cnt: Integer): String;

Implementation

Type
LpDWord = ^DWord;
PINPUTRECORD = ^INPUT_RECORD;
CONSOLE_FONT_INFOEX = Record
	cbSize : Cardinal;
	nFont : DWord;
	dwFontSize : Coord;
	FontFamily : Cardinal;
	FontWeight : Cardinal;
	FaceName : FACETYPE;
End;
PCONSOLE_FONT_INFOEX = ^CONSOLE_FONT_INFOEX;

SMALL_RECT = Record
	Left : SmallInt;
	Top : SmallInt;
	Right : SmallInt;
	Bottom : SmallInt;
End;
PSMALL_RECT = ^SMALL_RECT;

CONSOLE_SCREEN_BUFFER_INFO = Packed Record
	dwSize : Coord;
	dwCursorPosition : Coord;
	wAttributes : Word;
	srWindow : SMALL_RECT;
	dwMaximumWindowSize : Coord;
End;
PCONSOLE_SCREEN_BUFFER_INFO = ^CONSOLE_SCREEN_BUFFER_INFO;

CONSOLE_CURSOR_INFO = Record
	dwSize : DWord;
	bVisible : LongBool;
End;
PCONSOLE_CURSOR_INFO = ^CONSOLE_CURSOR_INFO;

SECURITY_ATTRIBUTES = Record
	nLength : DWord;
	lpSecurityDescriptor : Pointer;
	bInheritHandle : LongBool;
End;
PSECURITY_ATTRIBUTES = ^SECURITY_ATTRIBUTES;
HMenu = Handle;
HWnd = Handle;
UInt = Cardinal;

// $BF = ENABLE_WINDOW_INPUT Or ENABLE_MOUSE_INPUT Or ENABLE_EXTENDED_FLAGS Or ENABLE_ECHO_INPUT Or ENABLE_LINE_INPUT Or ENABLE_INSERT_MODE Or ENABLE_PROCESSED_INPUT
Const defaultMode: DWord = $BF;
Var
hStdin: Handle;
hStdout: Handle;
fdwSaveOldMode: DWord;
oldStdout: Handle;

Function GetConsoleMode(hConsoleHandle: Handle; lpMode: LpDWord): LongBool; External 'kernel32';
Function SetConsoleMode(hConsoleHandle: Handle; dwMode: DWord): LongBool; External 'kernel32';
Function GetCurrentConsoleFontEx(hConsoleOutput: Handle; bMaximumWindow: LongBool; lpConsoleCurrentFontEx: PCONSOLE_FONT_INFOEX): LongBool; External 'kernel32';
Function SetCurrentConsoleFontEx(hConsoleOutput: Handle; bMaximumWindow: LongBool; lpConsoleCurrentFontEx: PCONSOLE_FONT_INFOEX): LongBool; External 'kernel32';
Function GetConsoleScreenBufferInfo(hConsoleOutput: Handle; lpConsoleScreenBufferInfo: PCONSOLE_SCREEN_BUFFER_INFO): LongBool; External 'kernel32';
Function SetConsoleWindowInfo(hConsoleOutput: Handle; bAbsolute: LongBool; Const lpConsoleWindow: PSMALL_RECT): LongBool; External 'kernel32';
Function SetConsoleScreenBufferSize(hConsoleOutput: Handle; dwSize: Coord): LongBool; External 'kernel32';
Function ReadConsoleInputA(hConsoleInput: Handle; lpBuffer: PINPUTRECORD; nLength: DWord; lpNumberOfEventsRead: LpDWord): LongBool; External 'kernel32';
Function PeekConsoleInputA(hConsoleInput: Handle; lpBuffer: PINPUTRECORD; nLength: DWord; lpNumberOfEventsRead: LpDWord): LongBool; External 'kernel32';
Function FillConsoleOutputCharacterA(hConsoleOutput: Handle; cCharacter: Char; nLength: DWord; dwWriteCoord: Coord; lpNumberOfCharsWritten: LpDWord): LongBool; External 'kernel32';
Function FillConsoleOutputAttribute(hConsoleOutput: Handle; wAttribute: Word; nLength: DWord; dwWriteCoord: Coord; lpNumberOfAttrsWritten: LpDWord): LongBool; External 'kernel32';
Function SetConsoleTextAttribute(hConsoleOutput: Handle; wAttributes: Word): LongBool; External 'kernel32';
Function SetConsoleCursorPosition(hConsoleOutput: Handle; dwCursorPosition: Coord): LongBool; External 'kernel32';
Function WriteConsoleOutputCharacterA(hConsoleOutput: Handle; lpCharacter: PChar; nLength: DWord; dwWriteCoord: Coord; lpNumberOfCharsWritten: LpDWord): LongBool; External 'kernel32';
Function WriteConsoleOutputAttribute(hConsoleOutput: Handle; lpAttribute: Pointer; nLength: DWord; dwWriteCoord: Coord; lpNumberOfAttrsWritten: LpDWord): LongBool; External 'kernel32';
Function SetConsoleCursorInfo(hConsoleOutput: Handle; lpConsoleCursorInfo: PCONSOLE_CURSOR_INFO): LongBool; External 'kernel32';
Function FlushConsoleInputBuffer(hConsoleInput: Handle): LongBool; External 'kernel32';
Function SetConsoleOutputCP(wCodePageID: Cardinal): LongBool; External 'kernel32';
Function GetStdHandle(nStdHandle: DWord): Handle; External 'kernel32';
Function SetStdHandle(nStdHandle: DWord; hHandle: Handle): LongBool; External 'kernel32';
Function GetLastError(): DWord; External 'kernel32';
Function AttachConsole(dwProcessId: DWord): LongBool; External 'kernel32';
Function SetConsoleTitleA(lpConsoleTitle: PAnsiString): LongBool; External 'kernel32';
Function CreateConsoleScreenBuffer(dwDesiredAccess: DWord; dwShareMode: DWord; Const lpSecurityAttributes: PSECURITY_ATTRIBUTES; dwFlags: DWord; lpScreenBufferData: Pointer): Handle; External 'kernel32';
Function SetConsoleActiveScreenBuffer(hConsoleOutput: Handle): LongBool; External 'kernel32';
Function ReadConsoleA(hConsoleInput: Handle; lpBuffer: Pointer; nNumberOfCharsToRead: DWord; lpNumberOfCharsRead: LpDWord; pInputControl: Pointer): LongBool; External 'kernel32';
Function WriteConsoleA(hConsoleOutput: Handle; Const lpBuffer: Pointer; nNumberOfCharsToWrite: DWord; lpNumberOfCharsWritten: LpDWord; lpReserved: Pointer): LongBool; External 'kernel32';
Function WriteConsoleOutputA(hConsoleOutput: Handle; Const lpBuffer: PCHAR_INFO; dwBufferSize: Coord; dwBufferCoord: Coord; lpWriteRegion: PSMALL_RECT): LongBool; External 'kernel32';

Function StrDup(Const str: String; Const cnt: Integer): String;
Var
s: String;
i: Integer;
Begin
	s := '';
	For i := 1 To cnt Do
		s := s + str;
	StrDup := s;
End;


Procedure SetConsoleTitle(Const title: AnsiString);
Begin
	SetConsoleTitleA(PAnsiString(title));
End;

Procedure SetConsoleFont(Const FaceName: FACETYPE; Const x, y: Integer);
Var
FontInfo: CONSOLE_FONT_INFOEX;
Begin
	FontInfo.cbSize := sizeof(CONSOLE_FONT_INFOEX);
	FontInfo.nFont := 0;
	FontInfo.FontFamily := 54;
	FontInfo.FontWeight := 400;
	FontInfo.FaceName := FaceName;
	FontInfo.dwFontSize.X := x;
	FontInfo.dwFontSize.Y := y;
	SetCurrentConsoleFontEx(hStdout, False, @FontInfo);
End;

Procedure SetConsoleSize(Const Width: Integer; Const Height: Integer);
Var
ConsoleSize: SMALL_RECT;
CurrentInfo: CONSOLE_SCREEN_BUFFER_INFO;
Begin
	GetConsoleScreenBufferInfo(hStdout, @CurrentInfo);
	{ Set a buffer size bigger than the console size, this nearly guarantees the success Of setting console size }
	SetConsoleBuffer(Max(CurrentInfo.dwSize.X, Width), Max(CurrentInfo.dwSize.Y, Height));
	{ Then safely set the console size, will fail if the screen is too small }
	ConsoleSize.Top := 0;
	ConsoleSize.Left := 0;
	ConsoleSize.Right := Width - 1;
	ConsoleSize.Bottom := Height - 1;
	SetConsoleWindowInfo(hStdout, True, @ConsoleSize);
	{ Set the buffer size to be equal to the console size }
	SetConsoleBuffer(Width, Height);
End;

Procedure SetConsoleBuffer(Const Width: Integer; Const Height: Integer);
Var
BufferSize: Coord;
Begin
	{ Set the buffer size directly, this fails if the buffer size is too small }
	BufferSize.X := Width;
	BufferSize.Y := Height;
	SetConsoleScreenBufferSize(hStdout, BufferSize);
End;

Procedure PollConsoleInput(Var irInBuf: Array Of INPUT_RECORD; Const bufSize: DWord; Var cNumRead: DWord);
Begin
	ReadConsoleInputA(hStdin, irInBuf, bufSize, @cNumRead);
End;

Procedure PeekConsoleInput(Var irInBuf: Array Of INPUT_RECORD; Const bufSize: DWord; Var cNumRead: DWord);
Begin
	PeekConsoleInputA(hStdin, irInBuf, bufSize, @cNumRead);
End;

Procedure ClrScr();
Var
screen: Coord;
cCharsWritten: DWord;
CurrentInfo: CONSOLE_SCREEN_BUFFER_INFO;
Begin
	GetConsoleScreenBufferInfo(hStdout, @CurrentInfo);
	screen.X := 0;
	screen.Y := 0;
	FillConsoleOutputCharacterA(hStdout, ' ', CurrentInfo.dwSize.X * CurrentInfo.dwSize.Y, screen, @cCharsWritten);
	FillConsoleOutputAttribute(hStdout, CurrentInfo.wAttributes, CurrentInfo.dwSize.X * CurrentInfo.dwSize.Y, screen, @cCharsWritten);
	GoToXY(0, 0);
End;

Procedure SetConsoleColor(Const color: Word);
Begin
	SetConsoleTextAttribute(hStdout, color);
End;

Procedure TextBackground(Const color: Integer);
Var
CurrentInfo: CONSOLE_SCREEN_BUFFER_INFO;
Begin
	GetConsoleScreenBufferInfo(hStdout, @CurrentInfo);
	SetConsoleColor(CurrentInfo.wAttributes And 15 + color * 16);
End;

Procedure TextColor(Const color: Integer);
Var
CurrentInfo: CONSOLE_SCREEN_BUFFER_INFO;
Begin
	GetConsoleScreenBufferInfo(hStdout, @CurrentInfo);
	SetConsoleColor(CurrentInfo.wAttributes And 240 + color);
End;

Procedure GoToXY(Const x, y: Integer);
Var
Loc: Coord;
Begin
	Loc.X := x;
	Loc.Y := y;
	SetConsoleCursorPosition(hStdout, Loc);
End;

Procedure CursorOff();
Var
cursorInfo: CONSOLE_CURSOR_INFO;
Begin
	cursorInfo.bVisible := False;
	cursorInfo.dwSize := 100;
	SetConsoleCursorInfo(hStdout, @cursorInfo);
End;

Procedure CursorOn();
Var
cursorInfo: CONSOLE_CURSOR_INFO;
Begin
	cursorInfo.bVisible := True;
	cursorInfo.dwSize := 100;
	SetConsoleCursorInfo(hStdout, @cursorInfo);
End;

Procedure WriteAttr(Const attrs: Array Of Word; Const x, y: Integer);
Var
loc: Coord;
cAttrsWritten: DWord;
Begin
	loc.X := x;
	loc.Y := y;
	WriteConsoleOutputAttribute(hStdout, @attrs[0], High(attrs) + 1, loc, @cAttrsWritten);
End;

Function ReadKey(): Word;
Var
irInBuf: Array Of INPUT_RECORD;
cNumRead: DWord;
Begin
	SetLength(irInBuf, 1);
	Repeat
		ReadConsoleInputA(hStdin, @irInBuf[0], 1, @cNumRead);
	Until (irInBuf[0].EventType = 1) And (irInBuf[0].Event.KeyEvent.wVirtualKeyCode <> 0) And Not irInBuf[0].Event.KeyEvent.bKeyDown;
	ReadKey := irInBuf[0].Event.KeyEvent.wVirtualKeyCode;
End;

Procedure FlushInput();
Begin
	FlushConsoleInputBuffer(hStdin);
End;

Function CreateBuffer(): Handle;
Begin
	CreateBuffer := CreateConsoleScreenBuffer($40000000, 2, Nil, 1, Nil);
End;

Procedure SetActiveBuffer(ob: Handle);
Begin
	hStdout := ob;
	SetConsoleActiveScreenBuffer(hStdout);
	SetStdHandle(STD_OUTPUT_HANDLE, hStdout);
End;

Function GetActiveBuffer(): Handle;
Begin
	GetActiveBuffer := hStdout;
End;

// Added for this program
Procedure WriteDupAttr(Const X: Integer; Const Y: Integer; Const n: Integer);
Var
CurrentInfo: CONSOLE_SCREEN_BUFFER_INFO;
Attributes: Array Of Word;
i: Integer;
Begin
	GetConsoleScreenBufferInfo(hStdout, @CurrentInfo);
	SetLength(Attributes, n);
	For i := 0 To n - 1 Do
		Attributes[i] := CurrentInfo.wAttributes;
	WriteAttr(Attributes, X, Y);
End;

Procedure ReadEnter();
Var input: Word;
Begin
	Repeat
		input := ReadKey();
	Until input = $0D;
End;

Procedure ReadInt(Var i: Integer);
Var input: Word;
tmp: String;
j: Integer;
res: Integer;
Begin
	tmp := '';
	Repeat
		input := ReadKey();
		If (input in [$30..$39]) Or (input in [$60..$69]) Then
		Begin
			tmp := tmp + Chr(input Mod $30);
			Write(Chr(input Mod $30 + $30));
		End
		Else If (input = $08) And (Length(tmp) <> 0) Then
		Begin
			tmp := Copy(tmp, 1, Length(tmp) - 1);
			Write(#8 + ' ' + #8);
		End;
	Until (tmp <> '') And (input = $0D);
	res := 0;
	For j := 1 To Length(tmp) Do
	Begin
		res := res * 10;
		res := res + Ord(tmp[j]);
	End;
	i := res;
End;

Procedure OutputFastBuffer(Const fastBuffer: PCHAR_INFO; Const X, Y, Width, Height: Integer);
Var
bufSize: Coord;
bufCoord: Coord;
writeRegion: SMALL_RECT;
CurrentInfo: CONSOLE_SCREEN_BUFFER_INFO;
Begin
	GetConsoleScreenBufferInfo(hStdout, @CurrentInfo);
	bufSize.X := Width;
	bufSize.Y := Height;
	bufCoord.X := 0;
	bufCoord.Y := 0;
	writeRegion.Top := Y;
	writeRegion.Left := X;
	writeRegion.Right := X + Width - 1;
	writeRegion.Bottom := Y + Height - 1;
	WriteConsoleOutputA(hStdout, fastBuffer, bufSize, bufCoord, @writeRegion);
End;

Function DeleteMenu(hMenu: HMenu; uPosition: UInt; uFlags: UInt): LongBool; External 'user32';
Function GetSystemMenu(hWnd: HWnd; bRevert: LongBool): HMenu; External 'user32';
Function GetConsoleWindow(): HWnd; External 'kernel32';

Procedure DisableClose();
Begin
	DeleteMenu(GetSystemMenu(GetConsoleWindow(), False), $F060, $0);
End;

Initialization
	hStdin := GetStdHandle(STD_INPUT_HANDLE);
	hStdout := GetStdHandle(STD_OUTPUT_HANDLE);
	oldStdout := GetStdHandle(STD_OUTPUT_HANDLE);
	SetConsoleOutputCP(437);
	GetConsoleMode(hStdin, @fdwSaveOldMode);
	SetConsoleMode(hStdin, defaultMode);
Finalization
	SetConsoleMode(hStdin, fdwSaveOldMode);
	SetActiveBuffer(oldStdout);
End.