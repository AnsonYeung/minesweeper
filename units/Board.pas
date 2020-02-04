Unit Board;
Interface
Const
BoxWidth: Integer = 5;
BoxHeight: Integer = 3;
Type
BoxState = Record
	opened: Boolean;
	flagged: Boolean;
	value: -1..8;
End;
TBoard = Array Of Array Of BoxState;

Function InitBoard(Width: Integer; Height: Integer; numBombs: Integer; nx, ny: Integer): TBoard;
Procedure DrawBoard(Const gameBoard: TBoard);
Function MouseInBox(mousePosX, mousePosY, x, y: Integer): Boolean;
Procedure DrawBoxBackground(x, y: Integer; state: BoxState; hover: Boolean; output: Boolean);
Procedure DrawBoxContent(x, y: Integer; state: BoxState);
Procedure PrintPartialBoard(x, y, width, height: Integer);
Procedure PrintBoard();

Implementation
Uses Windows, Console;
Var
W, H: Integer;
fastBuffer: Array Of CHAR_INFO;
boxBuffer: Array Of CHAR_INFO;

Function InitBoard(Width: Integer; Height: Integer; numBombs: Integer; nx, ny: Integer): TBoard;
Var
gameBoard: TBoard;
i, j, k: Integer;
randomizer: Array Of Integer;
randomLen: Integer;
tmp, randLoc: Integer;
bombs: Array Of Array[0..1] Of Integer;
x, y: Integer;
Begin
	W := Width;
	H := Height;
	SetLength(fastBuffer, 0);
	SetLength(fastBuffer, W * H * BoxWidth * BoxHeight);
	SetLength(boxBuffer, BoxWidth * BoxHeight);
	randomLen := Width * Height;
	SetLength(randomizer, randomLen);
	SetLength(bombs, numBombs);
	SetLength(gameBoard, Width);
	For i := 0 To Width - 1 Do
	Begin
		SetLength(gameBoard[i], Height);
		For j := 0 To Height - 1 Do
		Begin
			gameBoard[i][j].value := 0;
			gameBoard[i][j].opened := false;
			gameBoard[i][j].flagged := false;
		End;
	End;
	For i := 0 To randomLen - 1 Do 
		randomizer[i] := i;
	randomizer[ny * Width + nx] := randomizer[randomLen - 1];
	Dec(randomLen);
	For i := 1 To numBombs Do
	Begin
		tmp := random(randomLen);
		randLoc := randomizer[tmp];
		randomizer[tmp] := randomizer[randomLen - 1];
		Dec(randomLen);
		bombs[i - 1][0] := randLoc Mod Width;
		bombs[i - 1][1] := randLoc Div Width;
		gameBoard[randLoc Mod Width][randLoc Div Width].value := -1;
	End;
	For i := 0 To numBombs - 1 Do
	Begin
		x := bombs[i][0];
		y := bombs[i][1];
		For j := -1 To 1 Do
			For k := -1 To 1 Do
				If ((x + j) In [0..Width-1]) And ((y + k) In [0..Height-1]) And (gameBoard[x + j][y + k].value <> -1) Then
					Inc(gameBoard[x + j][y + k].value);
	End;
	
	InitBoard := gameBoard;
End;

Procedure DrawBoxBackground(x, y: Integer; state: BoxState; hover: Boolean; output: Boolean);
Var
i, j: Integer;
color: Word;
Begin
	color := Red;
	If state.opened Then
		Case state.value Of
			1: color := LightBlue;
			2: color := Green;
			3: color := LightRed;
			4: color := LightPurple;
			5: color := Red;
			6: color := Blue;
			7: color := Black;
			8: color := Gray;
		End;
	If hover Then
		If Not state.opened And Not state.flagged Then
			color := Green * 16 + color
		Else
			color := Red * 16 + color
	Else
		If Not state.opened Then
			color := Gray * 16 + color
		Else If state.value = -1 Then
			color := LightRed * 16 + color
		Else
			color := White * 16 + color;
	For i := 0 To BoxHeight - 1 Do
		For j := 0 To BoxWidth - 1 Do
		Begin
			fastBuffer[x * BoxWidth + j + (y * BoxHeight + i) * W * BoxWidth].Attributes := color;
			boxBuffer[j + i * BoxWidth] := fastBuffer[x * BoxWidth + j + (y * BoxHeight + i) * W * BoxWidth];
		End;
	If output Then
		OutputFastBuffer(@boxBuffer[0], BoxWidth, BoxHeight, x * BoxWidth, y * BoxHeight, BoxWidth, BoxHeight);
End;

Procedure DrawBoxContent(x, y: Integer; state: BoxState);
Begin
	fastBuffer[x * BoxWidth + BoxWidth div 2 + (y * BoxHeight + BoxHeight div 2) * W * BoxWidth].AsciiChar := ' ';
	If state.flagged Then
		fastBuffer[x * BoxWidth + BoxWidth div 2 + (y * BoxHeight + BoxHeight div 2) * W * BoxWidth].AsciiChar := 'F'
	Else If state.opened Then
	Begin
		If state.value = -1 Then
			fastBuffer[x * BoxWidth + BoxWidth div 2 + (y * BoxHeight + BoxHeight div 2) * W * BoxWidth].AsciiChar := 'B'
		Else If state.value <> 0 Then
			fastBuffer[x * BoxWidth + BoxWidth div 2 + (y * BoxHeight + BoxHeight div 2) * W * BoxWidth].AsciiChar := Chr(48 + state.value);
	End;
End;

Procedure DrawBoard(Const gameBoard: TBoard);
Var
i, j: Integer;
column: Array Of BoxState;
Begin
	For i := Low(gameBoard) To High(gameBoard) Do
	Begin
		column := gameBoard[i];
		For j := Low(column) To High(column) Do
			DrawBoxBackground(i, j, column[j], False, False);
	End;
End;

Function MouseInBox(mousePosX, mousePosY, x, y: Integer): Boolean;
Begin
	MouseInBox := (mousePosX >= x * BoxWidth) And
		(mousePosX <= (x + 1) * BoxWidth - 1) And
		(mousePosY >= y * BoxHeight) And
		(mousePosY <= (y + 1) * BoxHeight - 1);
End;

Procedure PrintPartialBoard(x, y, width, height: Integer);
Begin
	OutputFastBuffer(@fastBuffer[0], W * BoxWidth, H * BoxHeight, x * BoxWidth, y * BoxHeight, width * BoxWidth, height * BoxHeight);
End;

Procedure PrintBoard();
Begin
	OutputFastBuffer(@fastBuffer[0], W * BoxWidth, H * BoxHeight, 0, 0, W * BoxWidth, H * BoxHeight);
End;

End.