{$mode objfpc}
Unit Menu;
Interface
Uses Console;
Const
WinWidth: Integer = 150;
Type
EKey = Procedure (Const event: KEY_EVENT_RECORD);
EMouse = Procedure (Const event: MOUSE_EVENT_RECORD);
MenuCb = Procedure (Const result: Integer);
TChoice = Array Of String;

Procedure EnterMenu(Var onKey: EKey; Var onMouse: EMouse; ponSelect: MenuCb; Var pchoices: TChoice);

Implementation
Var
choices: Array Of String;
choiceCnt: Integer;
choiceSelected: Integer;
onSelect: MenuCb;

Procedure WriteCenter(Const str : String; Const ln : Integer);
Begin
	GoToXY((WinWidth - Length(str)) Div 2, ln);
	Write(str);
End;

Function CoordInCenteredButton(Const mousePos: Coord; Const descrLen: Integer; Const ln: Integer): Boolean;
Begin
	CoordInCenteredButton := (mousePos.X >= (WinWidth - descrLen) Div 2 - 3) And
		(mousePos.X <= (WinWidth + descrLen) Div 2 + 2) And
		(mousePos.Y >= ln) And
		(mousePos.Y <= ln + 2);
End;

Procedure WriteButtonCenter(Const description: String; Const ln: Integer; Const hover: Boolean);
Var
len: Integer;
i: Integer;
Begin
	len := Length(description);
	If hover Then
	Begin
		TextBackground(Black);
		WriteCenter(v + StrDup(' ', len + 8) + v, ln);
		WriteCenter(v + StrDup(' ', len + 8) + v, ln + 1);
		WriteCenter(v + StrDup(' ', len + 8) + v, ln + 2);
		WriteCenter(cul + StrDup(h, len + 8) + cur, ln - 1);
		WriteCenter(cll + StrDup(h, len + 8) + clr, ln + 3);
		TextBackground(Yellow);
	End
	Else
	Begin
		TextBackground(Black);
		For i := ln - 1 To ln + 3 Do
			WriteCenter(StrDup(' ', len + 10), i);
		TextBackground(Blue);
	End;
	WriteCenter(StrDup(' ', len + 6), ln);
	WriteCenter('   ' + description + '   ', ln + 1);
	WriteCenter(StrDup(' ', len + 6), ln + 2);
End;

Procedure WriteMenuButton(Const i: Integer; Const hover: Boolean);
Begin
	WriteButtonCenter(choices[i], 20 + i * 5, hover);
End;

Procedure MenuEvent(Const event: KEY_EVENT_RECORD);
Begin
	If event.bKeyDown Then
	Case event.wVirtualKeyCode Of
		VK_UP: Begin
			WriteMenuButton(choiceSelected, False);
			choiceSelected := (choiceSelected - 1 + choiceCnt) Mod choiceCnt;
			WriteMenuButton(choiceSelected, True);
		End;
		VK_DOWN: Begin
			WriteMenuButton(choiceSelected, False);
			choiceSelected := (choiceSelected + 1) Mod choiceCnt;
			WriteMenuButton(choiceSelected, True);
		End;
	End
	Else
	Case event.wVirtualKeyCode Of
		VK_RETURN: onSelect(choiceSelected);
	End;
End;

Procedure MenuEvent(Const event: MOUSE_EVENT_RECORD);
Var button, i: Integer;
Begin
	If event.dwButtonState = 0 Then
	Begin
		button := -1;
		For i := 0 To choiceCnt - 1 Do
			If CoordInCenteredButton(event.dwMousePosition, Length(choices[i]), 20 + i * 5) Then
				button := i;
		Case event.dwEventFlags Of
			0: If button <> -1 Then onSelect(choiceSelected);
			MOUSE_MOVED:
			If (button <> -1) And (choiceSelected <> button) Then
			Begin
				WriteMenuButton(choiceSelected, False);
				WriteMenuButton(button, True);
				choiceSelected := button;
			End;
		End;
	End;
End;

Procedure EnterMenu(Var onKey: EKey; Var onMouse: EMouse; ponSelect: MenuCb; Var pchoices: TChoice);
Var
i: Integer;
Begin
	choices := pchoices;
	choiceCnt := Length(pchoices);
	choiceSelected := 0;
	onSelect := ponSelect;
	onKey := @MenuEvent;
	onMouse := @MenuEvent;
	For i := 0 To choiceCnt - 1 Do
		WriteMenuButton(i, choiceSelected = i);
End;

End.