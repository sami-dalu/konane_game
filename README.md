# Ultimate Konane
Welcome to Ultimate Konane, a twist on Konane.\
By Sinjin Cho-Tupua and Sami Dalu

# About Konane
Konane is a Hawaiian strategy board game wherein two players, black and white, take turns hopping over (capturing) each other’s pieces on a rectangular grid, similar to checkers. The pieces on the board are initially laid out in alternating colors. Black plays first by removing a black piece in one of the four corners or one of the four central squares. White responds by removing a white piece adjacent to the space left empty by black’s move. Captures are done orthogonally (i.e. horizontally and vertically rather than diagonally) to adjacent pieces. A player can make multiple captures in the same turn as long as the captures are in the same direction with the same piece. A player of Konane loses when they are unable to make any further captures. 

# How to play the game
In a Linux command line, navigate to your directory with the executable from the most recent release and run it using the following command:
``./konane``

You will then be greeted with the menu in the command line. When the command line presents you with a list of finite options, you can select your desired option by using either the arrow keys on your keyboard or by typing your option. In either case, confirm your choice by pressing the enter key. 

Afterwards, in the command line, you can choose to play against a bot with three difficulties (easy, medium, and hard) or multiplayer.


## Player v. Bot 
To play against a bot, simply select the "Player v. Bot" option. Then, 
1) Enter your name
2) Select your choice of game mode (Normal or Crazy)
3) Select a difficulty for the bot
4) Select the piece you would like to play (Black or White)
5) Finally, enter the height and then width of the board, and your game will start! 

## Player v. Player
For two players to play against one another, they must be on the same server. To start a server, 
1) On a host computer, run the command above and select "Player v. Player"
2) Select "Start a server"
3) Enter the port that the server should be listening on

After this server is created, each of the two human players can join a game on that server via the following steps:
1) Running the ``./konane`` command in their own terminal windows
2) Selecting "Player v. Player"
3) Selecting "Join a game"
4) Entering their name (nickname) for the game
5) Entering the host computer
6) Entering the port the server listens in on
7) Selecting the desired game mode (Normal or Crazy)
8) Entering the desired height of the board, and entering the desired width
9) If both players enter the same board dimensions and game mode, the game will begin!

# Crazy Mode
There are two modes in Ultimate Konane: Normal and Crazy. The normal mode works exactly as the "About Konane" section describes above. The Crazy mode has a few extra twists, including:
1) Volcanic eruptions, which will remove random pieces from the board and make squares not accessible
2) Plagues, which will infect certain pieces and doom them for removal after a few turns
3) Rotation of the board by 90 degrees clockwise
4) Flipping the colors of all pieces on the board (i.e black pieces become white pieces and white pieces become black pieces)
5) Monsters, who can eat adjacent pieces, breaking loose
6) Duplication serum leaking all across the board, making moving pieces leave a duplicate in their original starting spots

# Playing the game
When you are in the game and it is your turn to play, you will see a small blue circle over the pieces of yours which you can make a move from. Once you click one of these pieces, if you are making a capture (you will from your second move onwards), after selecting the piece you can make a move from, you will see a small yellow circle on the tiles you can leap to and thereby capture the adjacent opposite piece.
<img width="601" alt="image" align="bottom" src="https://github.com/user-attachments/assets/792cdaa0-cc76-44b3-8d10-16523578d699">

The starting position of the piece your opponent moved last turn will have a small light red circle over it, and their ending position will have a small regular red circle over it. 
<br clear="left">
<img width="600" alt="image" align="top" src="https://github.com/user-attachments/assets/af56c607-c8d2-4be2-a310-f3dcd1922d41">

You can make multiple captures in Konane, as long as the captures are in the same cardinal direction with the same piece. If you can make another capture but would like to end your turn, press the green "END TURN" button in the top right corner of the window.

If you would like to restart the game, you may do so at any time by pressing the red "RESTART GAME" button in the top left corner of the window. 
