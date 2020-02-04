{$mode objfpc}
Program TicTacToe;
Uses StopRunning, SysUtils, Classes, Math, Console, Board, Menu;
Const
oneSec: Real = 0.00001157407;

Type
EKey = Procedure (Const event: KEY_EVENT_RECORD);
EMouse = Procedure (Const event: MOUSE_EVENT_RECORD);
EWinBufferSize = Procedure (Const event: WINDOW_BUFFER_SIZE_RECORD);
EFocus = Procedure (Const event: FOCUS_EVENT_RECORD);
EMenu = Procedure (Const event: MENU_EVENT_RECORD);
TTimerThread = Class(TThread)
	Private
		currentTime: Integer;
	Protected
		Procedure Execute; Override;
	Public
		Constructor Create(createSuspended: Boolean);
End;

Var
irInBuf: Array[0..0] Of INPUT_RECORD;
cNumRead: DWord;
i: Integer;
onKey: EKey;
onMouse: EMouse;
onWinBufferSize: EWinBufferSize;
onFocus: EFocus;
onMenu: EMenu;

menuOptions, continueOptions, playOptions: Array Of String;
Width, Height, Bombs: Integer;
easyBest, mediumBest, hardBest: Real;

startTime: TDateTime;
disableRecord: Boolean;
gameDiff: Integer;
gameBoard: TBoard;
boxSelected: Coord;
boxOpened: Integer;
loadFromSave: Boolean;
remainingFlags: Integer;
TimerThread: TThread;

Constructor TTimerThread.Create(createSuspended: Boolean);
Begin
	Inherited Create(createSuspended);
	FreeOnTerminate := True;
End;

Procedure TTimerThread.Execute;
Begin
	currentTime := 0;
	If Not disableRecord Then
		While Not Terminated Do
		Begin
			GoToXY(Width * BoxWidth + 1, Height * BoxHeight - 2);
			Write('Time:           ');
			GoToXY(Width * BoxWidth + 1, Height * BoxHeight - 2);
			Write('Time: ', currentTime, 's');
			Inc(currentTime);
			Sleep(Ceil(1000 * (startTime + oneSec * currentTime - Time()) / oneSec));
		End;
End;

Procedure EnterConsole();
Begin
	SetConsoleFont('Lucida Console', 8, 16);
	SetConsoleSize(WinWidth, 45);
	TextBackground(Black);
	TextColor(White);
	ClrScr();
End;

Procedure NoopAll(); Forward;
Procedure Noop(Const event: KEY_EVENT_RECORD); Begin End;
Procedure Noop(Const event: MOUSE_EVENT_RECORD); Begin End;
Procedure Noop(Const event: WINDOW_BUFFER_SIZE_RECORD); Begin End;
Procedure Noop(Const event: FOCUS_EVENT_RECORD); Begin End;
Procedure Noop(Const event: MENU_EVENT_RECORD); Begin
{$IFNDEF FALLBACK}
	EnterConsole();
	Write('All settiings will be applied at runtime. Please don''t open the menu, press enter to close the program');
	ReadEnter();
	Halt(1);
{$ENDIF}
End;
Procedure RealNoop(Const event: MENU_EVENT_RECORD); Begin End;

Procedure NoopAll();
Begin
	onKey := @Noop;
	onMouse := @Noop;
	onWinBufferSize := @Noop;
	onFocus := @Noop;
	onMenu := @Noop;
End;

Procedure WriteCenter(Const str : String; Const ln : Integer);
Begin
	GoToXY((WinWidth - Length(str)) Div 2, ln);
	Write(str);
End;

Procedure WriteFileCenter(Const filename: String);
Var
textFile: Text;
i: Integer;
str: String;
Begin
	If Not FileExists(filename) Then
	Begin
		ClrScr();
		Write('The game file ' + filename + ' is missing, exiting');
		ReadEnter();
		Halt(1);
	End;
	Assign(textFile, filename);
	Reset(textFile);
	i := -1;
	While Not EOF(textFile) Do
	Begin
		Inc(i);
		ReadLn(textFile, str);
		WriteCenter(str, i);
	End;
	Close(textFile);
End;

Procedure SaveRecord(); Forward;
Procedure LoadRecord(); Forward;
Procedure ResetGame(); Forward;

Procedure InitProgram();
Begin
	Randomize();
	SetConsoleTitle('Minesweeper');
	{$IFNDEF FALLBACK}
	DisableClose();
	{$ENDIF}
	If FileExists('.running') Then
	Begin
		EnterConsole();
		Write('Another instance of this program is already running. If it is not the case, delete the file ".running" and run the program again.');
		ReadEnter();
		Halt(1);
	End;
	FileClose(FileCreate('.running'));
	CursorOff();
	EnterConsole();
	SetLength(menuOptions, 3);
	menuOptions[0] := 'Play';
	menuOptions[1] := 'Statistics';
	menuOptions[2] := 'Exit';
	SetLength(continueOptions, 3);
	continueOptions[0] := 'Continue Last Save';
	continueOptions[1] := 'Start new game';
	continueOptions[2] := 'Return to main menu';
	SetLength(playOptions, 5);
	playOptions[0] := 'Easy';
	playOptions[1] := 'Medium';
	playOptions[2] := 'Hard';
	playOptions[3] := 'Custom difficulty';
	playOptions[4] := 'Back to main menu';
	boxSelected.X := 0;
	boxSelected.Y := 0;
	If Not FileExists('records') Then
		ResetGame()
	Else
		LoadRecord();
End;

Procedure ResetGame();
Begin
	easyBest := 0;
	mediumBest := 0;
	hardBest := 0;
	SaveRecord();
End;

Procedure EnterMainMenu(); Forward;
Procedure EnterContinue(); Forward;
Procedure EnterPlay(); Forward;
Procedure EnterStat(); Forward;
Procedure EnterCustom(); Forward;
Procedure EnterGame(difficulty: Integer); Forward;
Procedure EnterGameEnd(win: Boolean); Forward;
Procedure SaveGame(); Forward;
Procedure LoadGame(); Forward;

Procedure MenuSelection(Const result: Integer);
Begin
	Case result Of
		0: EnterContinue();
		1: EnterStat();
		2: Halt(0);
	End;
End;

Procedure EnterMainMenu();
Begin
	EnterConsole();
	WriteFileCenter('scenes\menu_title.txt');
	NoopAll();
	EnterMenu(onKey, onMouse, @MenuSelection, menuOptions);
End;

Procedure ContinueSelection(Const result: Integer);
Begin
	Case result Of
		0: LoadGame();
		1: EnterPlay();
		2: EnterMainMenu();
	End;
End;

Procedure EnterContinue();
Begin
	If FileExists('save') Then
	Begin
		EnterConsole();
		WriteFileCenter('scenes\menu_title.txt');
		NoopAll();
		EnterMenu(onKey, onMouse, @ContinueSelection, continueOptions);
	End
	Else
		EnterPlay();
End;

Procedure PlaySelection(Const result: Integer);
Begin
	Case result Of
		0: EnterGame(0);
		1: EnterGame(1);
		2: EnterGame(2);
		3: EnterCustom();
		4: EnterMainMenu();
	End;
End;

Procedure EnterPlay();
Begin
	EnterConsole();
	WriteFileCenter('scenes\menu_title.txt');
	NoopAll();
	EnterMenu(onKey, onMouse, @PlaySelection, playOptions);
End;

Procedure EnterStat(); Begin
	EnterConsole();
	WriteLn('Best time records');
	WriteLn();
	If easyBest = 0 Then
		WriteLn('Easy: no record')
	Else
		WriteLn('Easy: ', easyBest:5:2, 's');
	If mediumBest = 0 Then
		WriteLn('Medium: no record')
	Else
		WriteLn('Medium: ', mediumBest:5:2, 's');
	If hardBest = 0 Then
		WriteLn('Hard: no record')
	Else
		WriteLn('Hard: ', hardBest:5:2, 's');
	WriteLn();
	WriteLn('Press enter to return to main menu.');
	ReadEnter();
	EnterMainMenu();
End;

Procedure EnterCustom(); Begin
	EnterConsole();
	CursorOn();
	Write('Enter the width of the board: ');
	ReadInt(Width);
	WriteLn();
	Write('Enter the height of the board: ');
	ReadInt(Height);
	WriteLn();
	Write('Enter the number of bombs of the board: ');
	ReadInt(Bombs);
	WriteLn();
	CursorOff();
	If (Width = 8) And (Height = 8) And (Bombs = 10) Then
	Begin
		WriteLn('Starting the difficulty easy...');
		Sleep(3000);
		EnterGame(0);
	End
	Else If (Width = 16) And (Height = 16) And (Bombs = 40) Then
	Begin
		WriteLn('Starting the difficulty medium...');
		Sleep(3000);
		EnterGame(1);
	End
	Else If (Width = 30) And (Height = 16) And (Bombs = 99) Then
	Begin
		WriteLn('Starting the difficulty hard...');
		Sleep(3000);
		EnterGame(2);
	End
	Else If Bombs < Width * Height Then
	Begin
		EnterGame(3);
	End
	Else
	Begin
		WriteLn('Invalid board configuation, returning to main menu in 3 seconds');
		Sleep(3000);
		EnterMainMenu();
	End;
End;

Procedure DrawBoxSelected(Const hover, output: Boolean); Forward;
Procedure GameEvent(Const event: KEY_EVENT_RECORD); Forward;
Procedure GameEvent(Const event: MOUSE_EVENT_RECORD); Forward;

Procedure EnterGame(difficulty: Integer); Begin
	gameDiff := difficulty;
	Case difficulty Of
		0: Begin
			Width := 8;
			Height := 8;
			Bombs := 10;
		End;
		1: Begin
			Width := 16;
			Height := 16;
			Bombs := 40;
		End;
		2: Begin
			Width := 30;
			Height := 16;
			Bombs := 99;
		End;
	End;
	boxOpened := 0;
	disableRecord := False;
	loadFromSave := False;
	remainingFlags := Bombs;
	NoopAll();
	onKey := @GameEvent;
	onMouse := @GameEvent;
	gameBoard := InitBoard(Width, Height, 0, 0, 0);
	SetConsoleSize(Width * BoxWidth + 30, Height * BoxHeight);
	TextBackground(Black);
	TextColor(White);
	ClrScr();
	GoToXY(Width * BoxWidth + 1, 0);
	Write('Arrow keys / mouse to move');
	GoToXY(Width * BoxWidth + 1, 1);
	Write('Enter / left click to select');
	GoToXY(Width * BoxWidth + 1, 2);
	Write('F key / right click to');
	GoToXY(Width * BoxWidth + 1, 3);
	Write('toggle flag');
	GoToXY(Width * BoxWidth + 1, 4);
	Write('S key to save game and quit');
	GoToXY(Width * BoxWidth + 1, 5);
	Write('Q key to quit game without');
	GoToXY(Width * BoxWidth + 1, 6);
	Write('saving');
	GoToXY(Width * BoxWidth + 1, 7);
	Write('Mid-click or left click and');
	GoToXY(Width * BoxWidth + 1, 8);
	Write('right click together to do');
	GoToXY(Width * BoxWidth + 1, 9);
	Write('chording');
	GoToXY(Width * BoxWidth + 1, Height * BoxHeight - 2);
	Write('Time: 0s');
	GoToXY(Width * BoxWidth + 1, Height * BoxHeight - 1);
	Write('Remaining Flags: ', remainingFlags);
	DrawBoard(gameBoard);
	If (boxSelected.X >= Width) Or (boxSelected.Y >= Height) Then
	Begin
		boxSelected.X := 0;
		boxSelected.Y := 0;
	End;
	DrawBoxSelected(True, False);
	PrintBoard();
End;

Procedure GameChangeSelection(Const X: Integer; Const Y: Integer); Forward;
Procedure GameSelection(flagging: Boolean; chording: Boolean); Forward;

Procedure GameEvent(Const event: KEY_EVENT_RECORD);
Begin
	If event.bKeyDown Then
	Case event.wVirtualKeyCode Of
		VK_UP: GameChangeSelection(boxSelected.X, (boxSelected.Y - 1 + Height) Mod Height);
		VK_DOWN: GameChangeSelection(boxSelected.X, (boxSelected.Y + 1) Mod Height);
		VK_LEFT: GameChangeSelection((boxSelected.X - 1 + Width) Mod Width, boxSelected.Y);
		VK_RIGHT: GameChangeSelection((boxSelected.X + 1) Mod Width, boxSelected.Y);
	End
	Else
	Case event.wVirtualKeyCode Of
		$46: GameSelection(True, False);
		$53: Begin
			SaveGame();
			Halt(0);
		End;
		$51: Halt(0);
		VK_RETURN: GameSelection(False, False);
	End;
End;

Procedure GameEvent(Const event: MOUSE_EVENT_RECORD);
Var
i, j, hoverX, hoverY: Integer;
Begin
	hoverX := -1;
	hoverY := -1;
	For i := 0 To Width - 1 Do
		For j := 0 To Height - 1 Do
			If MouseInBox(event.dwMousePosition.X, event.dwMousePosition.Y, i, j) Then
			Begin
				hoverX := i;
				hoverY := j;
			End;
	If (hoverX <> -1) And (hoverY <> -1) Then
	Case event.dwEventFlags Of
		0: If (event.dwButtonState And $04) <> $0 Then
			GameSelection(False, True)
		Else If (event.dwButtonState And $03) = $03 Then
			GameSelection(False, True)
		Else If (event.dwButtonState And $01) = $01 Then
			GameSelection(False, False)
		Else If (event.dwButtonState And $02) <> $0 Then
			GameSelection(True, False);
		MOUSE_MOVED:
		If (boxSelected.X <> hoverX) Or (boxSelected.Y <> hoverY) Then
			GameChangeSelection(hoverX, hoverY);
	End;
End;

Procedure DrawBoxSelected(Const hover, output: Boolean);
Begin
	DrawBoxBackground(boxSelected.X, boxSelected.Y, gameBoard[boxSelected.X][boxSelected.Y], hover, output);
End;

Procedure GameChangeSelection(Const X: Integer; Const Y: Integer);
Var tx, ty: Integer;
Begin
	If (boxSelected.X <> X) Or (boxSelected.Y <> Y) Then
	Begin
		tx := boxSelected.X;
		ty := boxSelected.Y;
		DrawBoxSelected(False, False);
		boxSelected.X := X;
		boxSelected.Y := Y;
		DrawBoxSelected(True, False);
		PrintPartialBoard(Min(tx, X), Min(ty, Y), Abs(tx - X) + 1, Abs(ty - Y) + 1);
	End;
End;

Procedure GameSelection(flagging: Boolean; chording: Boolean);
Var
st: Array Of Coord;
efLen: SizeInt;
c: Coord;
i, j, cnt: Integer;
success: Boolean;
newEl: Coord;
Begin
	If chording Then
	Begin
		If gameBoard[boxSelected.X][boxSelected.Y].opened Then
		Begin
			cnt := 0;
			For i := -1 To 1 Do
				For j := -1 To 1 Do
					If Not ((i = 0) And (j = 0)) And ((boxSelected.X + i) In [0..Width-1]) And ((boxSelected.Y + j) In [0..Height-1]) And gameBoard[boxSelected.X + i][boxSelected.Y + j].flagged Then
						Inc(cnt);
			If cnt = gameBoard[boxSelected.X][boxSelected.Y].value Then
			Begin
				newEl := boxSelected;
				success := True;
				For i := -1 To 1 Do
					For j := -1 To 1 Do
						If ((newEl.X + i) In [0..Width-1]) And ((newEl.Y + j) In [0..Height-1]) Then
							If Not gameBoard[newEl.X + i][newEl.Y + j].flagged And (gameBoard[newEl.X + i][newEl.Y + j].value = -1) Then
							Begin
								success := False;
							End
							Else
							Begin
								boxSelected.X := newEl.X + i;
								boxSelected.Y := newEl.Y + j;
								GameSelection(False, False);
								DrawBoxSelected(False, False);
								If Bombs + boxOpened = Width * Height Then
								Begin
									boxSelected := newEl;
									Exit();
								End;
							End;
				boxSelected := newEl;
				DrawBoxSelected(True, False);
				If Not success Then
				Begin
					EnterGameEnd(False);
				End;
			End;
		End;
	End
	Else If flagging And Not gameBoard[boxSelected.X][boxSelected.Y].opened Then
	Begin
		If gameBoard[boxSelected.X][boxSelected.Y].flagged Then
			Inc(remainingFlags)
		Else
			Dec(remainingFlags);
		GoToXY(Width * BoxWidth + 1, Height * BoxHeight - 1);
		Write('Remaining Flags:           ');
		GoToXY(Width * BoxWidth + 1, Height * BoxHeight - 1);
		Write('Remaining Flags: ', remainingFlags);
		gameBoard[boxSelected.X][boxSelected.Y].flagged := Not gameBoard[boxSelected.X][boxSelected.Y].flagged;
		DrawBoxContent(boxSelected.X, boxSelected.Y, gameBoard[boxSelected.X][boxSelected.Y]);
		DrawBoxSelected(True, False);
	End
	Else If Not gameBoard[boxSelected.X][boxSelected.Y].flagged And Not gameBoard[boxSelected.X][boxSelected.Y].opened Then
	Begin
		If boxOpened = 0 Then
		Begin
			gameBoard := InitBoard(Width, Height, Bombs, boxSelected.X, boxSelected.Y);
			startTime := Time();
			TimerThread := TTimerThread.Create(False);
		End;
		If gameBoard[boxSelected.X][boxSelected.Y].value = -1 Then
			EnterGameEnd(False)
		Else
		Begin
			SetLength(st, 1);
			st[0] := boxSelected;
			efLen := 1;
			Repeat
				c := st[efLen - 1];
				Dec(efLen);
				If Not gameBoard[c.X][c.Y].opened And Not gameBoard[c.X][c.Y].flagged Then
				Begin
					gameBoard[c.X][c.Y].opened := True;
					DrawBoxContent(c.X, c.Y, gameBoard[c.X][c.Y]);
					Inc(boxOpened);
					If gameBoard[c.X, c.Y].value = 0 Then
					Begin
						For i := -1 To 1 Do
							For j := -1 To 1 Do
								If Not ((i = 0) And (j = 0)) And ((c.X + i) In [0..Width - 1]) And ((c.Y + j) In [0..Height-1]) Then
								Begin
									newEl.X := c.X + i;
									newEl.Y := c.Y + j;
									If efLen = Length(st) Then
										SetLength(st, efLen * 2);
									st[efLen] := newEl;
									Inc(efLen);
								End;
					End;
				End;
			Until efLen = 0;
			DrawBoard(gameBoard);
			DrawBoxSelected(True, True);
		End;
		If boxOpened + Bombs = Width * Height Then
			EnterGameEnd(True);
	End;
	PrintBoard();
End;

Procedure GameEndEvent(Const event: KEY_EVENT_RECORD); Forward;

Procedure EnterGameEnd(win: Boolean);
Var i, j: Integer;
timeRecord: Real;
Begin
	timeRecord := (Time() - startTime) / oneSec;
	TimerThread.Terminate();
	If loadFromSave And FileExists('save') Then
	Begin
		DeleteFile('save');
	End;
	If Not win Then
		For i := 0 To Width - 1 Do
			For j := 0 To Height - 1 Do
			Begin
				If gameBoard[i][j].value = -1 Then
					gameBoard[i][j].opened := True;
				gameBoard[i][j].flagged := False;
			End;
	TextBackground(Black);
	TextColor(White);
	ClrScr();
	For i := 0 To Width - 1 Do
		For j := 0 To Height - 1 Do
			DrawBoxContent(i, j, gameBoard[i][j]);
	DrawBoard(gameBoard);
	TextBackground(Black);
	TextColor(White);
	GoToXY(Width * BoxWidth + 1, 0);
	If win Then
		Write('You win!')
	Else
		Write('You lose!');
	If win Then
	Begin
		GoToXY(Width * BoxWidth + 1, 1);
		If disableRecord Then
			Write('Timing is disabled')
		Else
		Begin
			Write('Time: ', timeRecord:5:2, 's');
			GoToXY(Width * BoxWidth + 1, 2);
			Case gameDiff Of
				0: If (timeRecord < easyBest) Or (easyBest = 0) Then
				Begin
					easyBest := timeRecord;
					Write('Best time for easy!');
				End
				Else
					Write('Time record: ', easyBest:5:2, 's');
				1: If (timeRecord < mediumBest) Or (mediumBest = 0) Then
				Begin
					mediumBest := timeRecord;
					Write('Best time for medium!');
				End
				Else
					Write('Time record: ', mediumBest:5:2, 's');
				2: If (timeRecord < hardBest) Or (hardBest = 0) Then
				Begin
					hardBest := timeRecord;
					Write('Best time for hard!');
				End
				Else
					Write('Time record: ', hardBest:5:2, 's');
			End;
			SaveRecord();
		End;
	End;
	GoToXY(Width * BoxWidth + 1, 3);
	Write('Press ''R'' to retry.');
	GoToXY(Width * BoxWidth + 1, 4);
	Write('Press ''Enter'' to return');
	GoToXY(Width * BoxWidth + 1, 5);
	Write('to main menu.');
	NoopAll();
	onKey := @GameEndEvent;
End;

Procedure GameEndEvent(Const event: KEY_EVENT_RECORD);
Begin
	If Not event.bKeyDown Then
		If event.wVirtualKeyCode = $0D Then
			EnterMainMenu()
		Else If event.wVirtualKeyCode = $52 Then
			EnterGame(gameDiff);
End;

Procedure SaveGame();
Var
f: Text;
i, j: Integer;
Begin
	Assign(f, 'save');
	Rewrite(f);
	Write(f, Chr(gameDiff));
	Write(f, Chr(Width Div 256), Chr(Width Mod 256));
	Write(f, Chr(Height Div 256), Chr(Height Mod 256));
	Write(f, Chr(Bombs Div 256), Chr(Bombs Mod 256));
	Write(f, Chr(boxOpened Div 256), Chr(boxOpened Mod 256));
	For i := 0 To Width - 1 Do
		For j := 0 To Height - 1 Do
		Begin
			If gameBoard[i][j].opened Then
				Write(f, Chr(1))
			Else If gameBoard[i][j].flagged Then
				Write(f, Chr(2))
			Else
				Write(f, Chr(0));
			If gameBoard[i][j].value = -1 Then
				Write(f, Chr(10))
			Else
				Write(f, Chr(gameBoard[i][j].value));
		End;
	Close(f);
End;

Procedure LoadGame();
Var
f: Text;
c1, c2: Char;
i, j: Integer;
Begin
	Assign(f, 'save');
	Reset(f);
	Read(f, c1);
	gameDiff := Ord(c1);
	Read(f, c1, c2);
	Width := Ord(c1) * 256 + Ord(c2);
	Read(f, c1, c2);
	Height := Ord(c1) * 256 + Ord(c2);
	Read(f, c1, c2);
	Bombs := Ord(c1) * 256 + Ord(c2);
	EnterGame(gameDiff);
	Read(f, c1, c2);
	boxOpened := Ord(c1) * 256 + Ord(c2);
	For i := 0 To Width - 1 Do
		For j := 0 To Height - 1 Do
		Begin
			Read(f, c1);
			gameBoard[i][j].opened := False;
			gameBoard[i][j].flagged := False;
			If Ord(c1) = 1 Then
				gameBoard[i][j].opened := True
			Else If Ord(c1) = 2 Then
				gameBoard[i][j].flagged := True;
			Read(f, c1);
			If Ord(c1) = 10 Then
				gameBoard[i][j].value := -1
			Else
				gameBoard[i][j].value := Ord(c1);
			DrawBoxContent(i, j, gameBoard[i][j]);
		End;
	Close(f);
	DrawBoard(gameBoard);
	DrawBoxSelected(True, True);
	disableRecord := True;
	loadFromSave := True;
End;

Procedure SaveRecord();
Var
f: Text;
Begin
	Assign(f, 'records');
	Rewrite(f);
	WriteLn(f, easyBest);
	WriteLn(f, mediumBest);
	WriteLn(f, hardBest);
	Close(f);
End;

Procedure LoadRecord();
Var
f: Text;
Begin
	Assign(f, 'records');
	Reset(f);
	ReadLn(f, easyBest);
	ReadLn(f, mediumBest);
	ReadLn(f, hardBest);
	Close(f);
End;

Begin
	InitProgram();
	SetConsoleSize(WinWidth, 45);
	ClrScr();
	WriteFileCenter('scenes\splash.txt');
	Sleep(3000);
	EnterMainMenu();
	{ Infinite Event Loop }
	While True Do
	Begin
		PollConsoleInput(irInBuf, 1, cNumRead);
		For i := 0 To cNumRead - 1 Do
		Case irInBuf[i].EventType Of
			1: onKey(irInBuf[i].Event.KeyEvent);
			2: onMouse(irInBuf[i].Event.MouseEvent);
			4: onWinBufferSize(irInBuf[i].Event.WindowBufferSizeEvent);
			16: onFocus(irInBuf[i].Event.FocusEvent);
			8: onMenu(irInBuf[i].Event.MenuEvent);
		End;
	End;
End.