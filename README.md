# The Goose Game Kata
The Goose Game is a game where two or more players move pieces on a game board containing 63 squares by rolling a die. 
The aim of the game is to be the first one to reach the last square.

This repository is an attempt to solve the programming Kata presented here: 
[goose-game-kata](https://github.com/xpeppers/goose-game-kata).

#Instructions
## Buillding and running
The project uses sbt as the build tool. Therefore the usual commands apply.

1. Launch sbt in the repository : `sbt`
2. Compile: `compile`
3. Run the tests: `test`
4. Launch the program: `runMain ch.binaerbaum.goose.game.GooseGameApp`

Alternatively the application can be run within the IDE, e.g. IntelliJ.

## Input commands
- Type `add player <name>` to add a player.
- Type `move <name> 3, 4` to move the player with a specific dice roll.
- Type `move <name>` if you want the system to move the player.
- Type "quit" at any moment to quit the game.
- Have Fun!

#Limitations
- The system does not keep track of the players turns, therefore a player can play as many times in a row as he or she 
may want.

#Sample output
```
[info] Running ch.binaerbaum.goose.game.GooseGameApp 
Type "add player <name>" to add a player.
Type "move <name> 3, 4" to move the player with a specific dice roll.
Type "move <name> if you want the system to move the player.
Type "quit" at any moment to quit the game.
Have Fun!
add player Pippo
players: Pippo
add player Pluto
players: Pippo, Pluto
move Pippo
Pippo rolls 2, 4. Pippo moves from Start to The Bridge. Pippo jumps to 12
move Pluto
Pluto rolls 6, 2. Pluto moves from Start to 8
move Pippo
Pippo rolls 3, 1. Pippo moves from 12 to 16
move Pluto
Pluto rolls 1, 6. Pluto moves from 8 to 15
quit
```