# General Chess Puzzle Solver and Generator

An open source console program written in [Free Pascal](https://www.freepascal.org/) aimed to solve and generate chess puzzles like on the site https://www.puzzle-chess.com/, which was the main inspiration for it.

It's able to solve and generate [Solitaire](https://www.puzzle-chess.com/solitaire-chess-4/), [Solo](https://www.puzzle-chess.com/solo-chess-4/), [Melee](https://www.puzzle-chess.com/chess-melee-4/) and Binary chess puzzles. Every generated puzzle is unique (see Uniqueness Rule).

*It goes without saying, but **don't** use this program to solve the puzzles on the site for you. That's cheating.*

## Solitaire Chess

Solitaire chess has following rules:

- Pieces move as standard chess pieces
- Only capture moves are allowed
- The king is capturable
- Goal of the puzzle is to end up with one single piece on the board

## Solo Chess

Solo chess has following rules:

- Pieces move as standard chess pieces
- Only capture moves are allowed
- Each piece can only move twice
- The king is *not* capturable
- Goal of the puzzle is to end up with only the king on the board

## Chess Melee

Chess Melee was invented by Atanas Georgiev, the creator of the [puzzle site](https://www.puzzle-chess.com/chess-melee-4/) and has following rules:

- Pieces move as standard chess pieces
- White begins
- Only capture moves are allowed
- Each piece can only capture pieces opposite of its own colour
- Goal of the puzzle is to end up with one single piece on the board

Note: Puzzles generated have no kings.

## Binary Chess

Binary Chess was invented by me, [novemdecillions](https://github.com/novemdecillions/) and combines Solo and Melee. It has following rules:

- Pieces move as standard chess pieces
- White begins
- Only capture moves are allowed
- Each piece can only capture pieces opposite of its own colour
- Each piece can only move twice
- Each colour has one king
- The kings are *not* capturable
- Goal of the puzzle is to end up with two kings on the board next to each other (in capturable range)

## Usage

One can either use the precombiled exe-file (for Windows) or compile the program themselves (like for other operating systems). For the latter, either compile with [Free Pascal](https://www.freepascal.org/) or use the [Lazarus IDE](https://www.lazarus-ide.org/).

The program is an interactive console program. At the beginning, one chooses the puzzle type (or options), then whether to solve or generate.

When solving, it asks for the [FEN](https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation) of the position, then outputs the solution in the long algebraic notation without the piece symbols. 

When generating puzzles, it asks for the size of the puzzle to generate (size meaning piece amounts at the start), then how many puzzles it should generate. Afterwards, it asks whether to save the puzzle in a file. If yes, then the puzzles are saved in a file 'Puzzles.txt' in the same directory as the program. The file gets appended, so previously generated puzzles are not overriden. If the file doesn't exists, it tries to create it and then save the puzzles there. Puzzles are saved in the file as soon as they are created, so it's possible to close the program during generation and still have the already generated puzzles available. 

During generation, it shows progress by outputting which puzzle the program has already generated. If showing progress in the options has been chosen (default is off), then additional steps during generation like checking uniqueness or adding pieces are also shown.

When finished, it outputs all generated puzzles on the console in the FEN format.

*Note:* While the program was written with the intend to be as fast as possible, puzzle generation's time usage grows exponentially with each size. Check first how long each puzzle size takes to generate before generation a huge amount of them (like sizes higher than 11).

Minimal and maximal sizes are hard-coded. If you intend to generate bigger ones, change the constant 'MaxPuzzleSize' in the interface of the unit and recompile. However, the maximal sizes are already generous and will take a long time to generate a puzzle (not even mentioning how hard the generated puzzle becomes).

## Uniqueness Rule

Each generated puzzle is unique. This means that there exists only one solution for each puzzle. Different move order is allowed, as long as the same moves have to be made.

## Additional Notes

Currently, there is no other method (known to me) than to solve the puzzles on [Lichess editor](https://lichess.org/editor) and move the pieces there around. Feel free to create an gui or site where the puzzles can be solved according to the rules.
