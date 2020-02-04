# Minesweeper
A console version of minesweeper written in Pascal.  
This program uses the event-driven design and it only supports Windows.  
It is developed and tested under Windows 10.  
The repository of this program is [here](https://github.com/AnsonYeung/minesweeper)  

## Instructions
All console settings will be set by the program at runtime.  
This program can receive both mouse and keyboard input.  
If the program complains about another instance running, the file `.running` can be deleted to avoid this behaviour.  
Timing will be disabled for games that are saved and reopened to prevent cheating behaviour.  

### Chording
Chording is a technique in minesweeper that allows the user to open up nearby cells quickly.  
Use chording on a opened cell and the surrounding bombs of it is all marked by a flag and it will open up cells.

### In-game instructions
Arrow keys / mouse to move  
Enter / left click to select  
F key / right click to toggle flag  
S key to save game and quit  
Q key to quit game without saving  
Mid-click or left click and right click together to do chording  

## Compiling
The program can be compiled using fpc 3.0.4  
With the flag `-dFALLBACK`, the program will allow closing the program with button and opening the console setting.  
However, The program might not work properly if it is closed with that button.  
Compiling with the flag `-dRELEASE` is recommended.  