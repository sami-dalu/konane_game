# Ultimate Konane
Welcome to Ultimate Konane, a twist on Konane. 
by Sinjin Cho-Tupua and Sami Dalu (-7900) 

# About Konane
Konane is a Hawaiian strategy board game wherein two players, black and white, take turns hopping over (capturing) each other’s pieces on a rectangular grid, similar to checkers. The pieces on the board are initially laid out in alternating colors. Black plays first by removing a black piece in one of the four corners or one of the four central squares. White responds by removing a white piece adjacent to the space left empty by black’s move. Captures are done orthogonally (i.e. horizontally and vertically rather than diagonally) to adjacent pieces. A player can make multiple captures in the same turn as long as the captures are in the same direction with the same piece. A player of Konane loses when they are unable to make any further captures. 

# How to play the game
In the command line, execute the executable from the most recent release using the following command:
``dune exec konane.exe``

You will then be greeted with the menu in the command line. When the command line presents you with a list of finite options, you can select your desired option by using either the arrow keys on your keyboard or by typing your option. In either case, confirm your choice using enter. 

Afterwards, in the command line, you can choose to play against a bot with three difficulties (easy, medium, and hard) or multiplayer.


## Player v. Bot 
To play against a bot, simply select the "Player v. Bot" option. Enter your name, select your choice of game mode (Normal or Crazy), select a difficulty for the bot, and select the piece you would like to play (Black or White). Finally, enter the height and then width of the board, and your game will start! 

## Player v. Player
For two players to play against one another, they must be on the same server. To start a server, on a host computer, run the command above and select "Player v. Player". Then, select "Start a server" and enter the port that the server should be listening on. 

After this server is created, each of the two human players can join this game by running the konane executable, selecting "Player v. Player", selecting "Join a game", entering the port the server listens in on, entering the host computer, selecting the desired game mode (Normal or Crazy), entering the desired height of the board, and entering the desired width. If both players enter the same board dimensions and game mode, the game will begin!

# Crazy Mode
There are two modes in Ultimate Konane: Normal and Crazy. The normal mode works exactly was the "About Konane" section describes above. The Crazy mode has a few extra twists, including volcanic eruptions, which will remove random pieces from the board and make squares not accessible, plagues, which will infect certain pieces and doom them to removal after a number of turns,
