# oChess

Alan Pascual (ap835), Stephanie Shum (ss2972), Catherine Zhou (cz284)

## System Description

### Core Vision:

We have implemented a spinoff of the classic two-player game of chess in OCaml by allowing the player to personalize a custom piece, customize the board, and providing power-ups.

### Key Features:

* Graphical user interface with chess board displaying the current board.
* Multiplayer functionality allowing two players to take turns and make moves, on the same computer.
* Game engine interpreting player input as changes in the board.
* Customizability for game pieces. Ability to change movement patterns. (GUI overlay)
* Customizability for board layout. Ability to add gaps, and change board size. (GUI overlay)
* Power-ups that can change the state of the board.

### Narrative Description:

The chess game includes a GUI with a classic chess board of white and black pieces, showing the current state of the board. Players can use the GUI to specify what pieces they would like to move and to where. These changes will be processed by a game engine that identifies it as simple movement, promotion, castling, possession, or victory and alters the board state to reflect it. The game will be played on a single computer, with both players switching off turns.

In addition to being able to play the classic game of chess, the player will also be able to customize a game piece and the board layout. Such modifications include a making a customizable ‘camel’ piece, changing the number and placement of pieces, adding gaps in the board, and changing the size of the board (using the gaps). They would make modifications by using the GUI to represent the changes to the board and pieces. We also implemented highlighting of legal moves that custom pieces can move to.

## System Design

Our design is based off of the model-view-controller architectural pattern.

The following are the important modules that will be implemented:

* **Command:** A compilation unit that communicates with the GUI to retrieve commands to change the board state.
* **State:** A compilation unit used to represent the state of the game, including the placement of the pieces, whose turn it is, and pieces that have been captured, etc. The board shape is in the state missing field.
* **Models:** A compilation unit for chess piece movement - contains types for pieces containing valid movement patterns for each, powerups, etc.
* **GUI:** The module that is responsible for the graphical user interface depicting the current state of the board and pieces. Also doubles as our game engine.

## Module Design

The compilations units mentioned above include .mli files: command.mli, state.mli, and models.mli.

## Data

The system will have to maintain the state of the game, including where the pieces are located and other attributes related to each piece. Data structures we will use are records to store information defining the game state and each chess piece; tuples to define piece position by row and column and store the score of player A and B respectively; variants to define player commands, piece type (knight, queen, etc.), and player color; and lists to represent the active pieces currently on the board and the missing spots of a 12x12 board which defines the actual play area. The record representing state stores the list of missing spots of the 12x12 board, the list of active pieces on the board, the list of piece names associated with their list of possible moves, the list of captured pieces, the color of the current player, information on whether a promotion is being done, the position of the top row, the position of the bottom row, the turn number, the score tuple, the position of the white king, the position of the black king, a list of valid power-ups, whether a king is in check, and whether a king is in checkmate. The active board is just the area in the 12x12 grid that is not in the missing list field of state.

While there are 5 different command variants, only 2 are needed to fulfill the purpose of the GUI.

The move command will update the piece specified by its original position (the first of the tuple) to be associated with the new position (the second of the tuple) in the list of pieces in the state. 

The promotion command on the other hand contains a piece name to replace the promoting pawn.

Data about the starting game is stored in a json string. This contains all the information that state needs but slightly less as state can inference some information in the init_json.

## External Dependencies

Third-party libraries we will use are:
* **js_of_ocaml:** for creating a GUI in a web browser by compiling OCaml bytecode to Javascript
* **Yojson:** for creating and parsing JSON strings
* **oUnit:** for unit testing

## Run Instructions

Before making, must install dependencies.

In command line, type

**opam install js_of_ocaml**

**opam install js_of_ocaml-ocamlbuild**

**opam install js_of_ocaml-camlp4**

Once dependencies are installed:

**make js**

Finally open chessapp.html 

To run again:

**make clean**

and follow instructions above again.

