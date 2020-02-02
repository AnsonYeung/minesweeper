{$mode objfpc}
Program TicTacToe;
Uses StopRunning, SysUtils, Console, Board, Menu;
Type
EKey = Procedure (Const event: KEY_EVENT_RECORD);
EMouse = Procedure (Const event: MOUSE_EVENT_RECORD);
EWinBufferSize = Procedure (Const event: WINDOW_BUFFER_SIZE_RECORD);
EFocus = Procedure (Const event: FOCUS_EVENT_RECORD);
EMenu = Procedure (Const event: MENU_EVENT_RECORD);

Var
irInBuf: Array[0..127] Of INPUT_RECORD;
cNumRead: DWord;
i: Integer;
onKey: EKey;
onMouse: EMouse;
onWinBufferSize: EWinBufferSize;
onFocus: EFocus;
onMenu: EMenu;

menuOptions, playOptions: Array Of String;
Width, Height, Bombs: Integer;
easyBest, mediumBest, hardBest: Integer;

gameDiff: Integer;
gameBoard: TBoard;
boxSelected: Coord;
boxOpened: Integer;

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

Procedure ResetGame(); Forward;

Procedure InitProgram();
Var
c1, c2: Char;
textFile : Text;
Begin
	Randomize();
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
	SetLength(playOptions, 5);
	playOptions[0] := 'Easy';
	playOptions[1] := 'Medium';
	playOptions[2] := 'Hard';
	playOptions[3] := 'Custom difficulty';
	playOptions[4] := 'Back to main menu';
	boxSelected.X := 0;
	boxSelected.Y := 0;
	If Not FileExists('.minesweeper') Then
		ResetGame()
	Else
	Begin
		Assign(textFile, '.minesweeper');
		Reset(textFile);
		Read(textFile, c1, c2);
		easyBest := Ord(c1) * 256 + Ord(c2);
		Read(textFile, c1, c2);
		mediumBest := Ord(c1) * 256 + Ord(c2);
		ReadLn(textFile, c1, c2);
		hardBest := Ord(c1) * 256 + Ord(c2);
		Close(textFile);
	End;
End;

Procedure ResetGame();
Var
textFile : Text;
Begin
	easyBest := 0;
	mediumBest := 0;
	hardBest := 0;
	Assign(textFile, '.minesweeper');
	Rewrite(textFile);
	WriteLn(textFile, StrDup(Chr(0), 6));
	Close(textFile);
End;

Procedure EnterMainMenu(); Forward;
Procedure EnterPlay(); Forward;
Procedure EnterStat(); Forward;
Procedure EnterCustom(); Forward;
Procedure EnterGame(difficulty: Integer); Forward;
Procedure EnterGameEnd(win: Boolean); Forward;
// Procedure EnterGameEnd(Const player: Integer); Forward;

Procedure MenuSelection(Const result: Integer);
Begin
	Case result Of
		0: EnterPlay();
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
		WriteLn('Easy: ', easyBest, 's');
	If mediumBest = 0 Then
		WriteLn('Medium: no record')
	Else
		WriteLn('Medium: ', mediumBest, 's');
	If hardBest = 0 Then
		WriteLn('Hard: no record')
	Else
		WriteLn('Hard: ', hardBest, 's');
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
	// TODO: check difficulty valid
	EnterGame(3);
End;

Procedure DrawBoxSelected(Const hover: Boolean); Forward;
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
	NoopAll();
	onKey := @GameEvent;
	onMouse := @GameEvent;
	gameBoard := InitBoard(Width, Height, 0, 0, 0);
	SetConsoleSize(Width * BoxWidth + 30, Height * BoxHeight);
	TextBackground(Black);
	TextColor(White);
	ClrScr();
	// TODO: Write instructions
	DrawBoard(gameBoard);
	If (boxSelected.X >= Width) Or (boxSelected.Y >= Height) Then
	Begin
		boxSelected.X := 0;
		boxSelected.Y := 0;
	End;
	DrawBoxSelected(True);
End;

Procedure GameChangeSelection(Const X: Integer; Const Y: Integer); Forward;
Procedure GameSelection(flagging: Boolean); Forward;

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
		$46: GameSelection(True);
		VK_RETURN: GameSelection(False);
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
		0: If (event.dwButtonState And $01) <> $0 Then
			GameSelection(False)
		Else If (event.dwButtonState And $02) <> $0 Then
			GameSelection(True);
		MOUSE_MOVED:
		If (boxSelected.X <> hoverX) Or (boxSelected.Y <> hoverY) Then
			GameChangeSelection(hoverX, hoverY);
	End;
End;

Procedure DrawBoxSelected(Const hover: Boolean);
Begin
	DrawBoxBackground(boxSelected.X, boxSelected.Y, gameBoard[boxSelected.X][boxSelected.Y], hover);
End;

Procedure GameChangeSelection(Const X: Integer; Const Y: Integer);
Begin
	If (boxSelected.X <> X) Or (boxSelected.Y <> Y) Then
	Begin
		DrawBoxSelected(False);
		boxSelected.X := X;
		boxSelected.Y := Y;
		DrawBoxSelected(True);
	End;
End;

Procedure GameSelection(flagging: Boolean);
Var
st: Array Of Coord;
efLen: SizeInt;
c: Coord;
i, j: Integer;
newEl: Coord;
Begin
	// TODO: Chording?
	If flagging Then
	Begin
		gameBoard[boxSelected.X][boxSelected.Y].flagged := Not gameBoard[boxSelected.X][boxSelected.Y].flagged;
		DrawBoxContent(boxSelected.X, boxSelected.Y, gameBoard[boxSelected.X][boxSelected.Y]);
		DrawBoxSelected(True);
	End
	Else If Not gameBoard[boxSelected.X][boxSelected.Y].flagged And Not gameBoard[boxSelected.X][boxSelected.Y].opened Then
	Begin
		If boxOpened = 0 Then
			gameBoard := InitBoard(Width, Height, Bombs, boxSelected.X, boxSelected.Y);
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
			DrawBoxSelected(True);
		End;
		If boxOpened + Bombs = Width * Height Then
			EnterGameEnd(True);
	End;
End;

Procedure EnterGameEnd(win: Boolean);
Var i, j: Integer;
input: Word;
Begin
	If Not win Then
		For i := 0 To Width - 1 Do
			For j := 0 To Height - 1 Do
			Begin
				If gameBoard[i][j].value = -1 Then
					gameBoard[i][j].opened := True;
				gameBoard[i][j].flagged := False;
				DrawBoxContent(i, j, gameBoard[i][j]);
			End;
	DrawBoard(gameBoard);
	TextBackground(Black);
	TextColor(White);
	GoToXY(Width * BoxWidth + 1, 0);
	If win Then
		Write('You win!')
	Else
		Write('You lose!');
	GoToXY(Width * BoxWidth + 1, 3);
	Write('Press ''R'' to retry.');
	GoToXY(Width * BoxWidth + 1, 4);
	Write('Press ''Enter'' to return');
	GoToXY(Width * BoxWidth + 1, 5);
	Write('to main menu.');
	Repeat
		input := ReadKey();
	Until (input = $0D) Or (input = $52);
	If input = $0D Then
		EnterMainMenu()
	Else
		EnterGame(gameDiff);
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
		PollConsoleInput(irInBuf, 128, cNumRead);
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