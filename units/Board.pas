Unit Board;
Interface
Const
BoxWidth: Integer = 5;
BoxHeight: Integer = 3;
Type
BoxState = Record
	opened: Boolean;
	value: -1..8;
End;
TBoard = Array Of Array Of BoxState;

Function InitBoard(Width: Integer; Height: Integer; numBombs: Integer): TBoard;
Procedure DrawBoard(Const gameBoard: TBoard);
Function MouseInBox(mousePosX, mousePosY, x, y: Integer): Boolean;
Procedure DrawBoxBackground(x, y: Integer; state: BoxState; hover: Boolean);
Procedure DrawBoxContent(x, y: Integer; state: BoxState);

Implementation
Uses Console;

Function InitBoard(Width: Integer; Height: Integer; numBombs: Integer): TBoard;
Var
gameBoard: TBoard;
i, j, k: Integer;
randomizer: Array Of Integer;
randomLen: Integer;
tmp, randLoc: Integer;
bombs: Array Of Array[0..1] Of Integer;
x, y: Integer;
Begin
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
		End;
	End;
	For i := 0 To randomLen - 1 Do 
		randomizer[i] := i;
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

Procedure DrawBoxBackground(x, y: Integer; state: BoxState; hover: Boolean);
Var
i: Integer;
Begin
	TextColor(Black);
	If hover Then
		If Not state.opened Then
			TextBackground(Green)
		Else
			TextBackground(Red)
	Else
		If Not state.opened Then
			TextBackground(Gray)
		Else
			TextBackground(White);
	For i := 0 To BoxHeight - 1 Do
		WriteDupAttr(x * BoxWidth, y * BoxHeight + i, BoxWidth);
End;

Procedure DrawBoxContent(x, y: Integer; state: BoxState);
Begin
	GoToXY(x * BoxWidth + BoxWidth div 2, y * BoxHeight + BoxHeight div 2);
	// TODO: add different colors
	If state.value = -1 Then
		Write('B')
	Else If state.value <> 0 Then
		Write(state.value);
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
			DrawBoxBackground(i, j, column[j], False);
	End;
End;

Function MouseInBox(mousePosX, mousePosY, x, y: Integer): Boolean;
Begin
	MouseInBox := (mousePosX >= x * BoxWidth) And
		(mousePosX <= (x + 1) * BoxWidth - 1) And
		(mousePosY >= y * BoxHeight) And
		(mousePosY <= (y + 1) * BoxHeight - 1);
End;

End.