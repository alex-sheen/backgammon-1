# Backgammon (CMSC 15100 Final Project)
Typed/Racket multiplayer backgammon program with support for undoing moves and storing/loading games through serialization—converts structured data into linear string representations and back. Makes ample use of recursion and linked lists. Developed in two styles, **Original** and **Classic**, as pictured below.

## Original
```
(run original)
```
![Initial game, Original style](images/initial.png)
![Final game, Original style](images/final.png)

## Classic
![Classic style](images/classic-style.png)

## How to Run
1. Run the **project3-revised.rkt** file in the [DrRacket](https://download.racket-lang.org) programming environment.
2. In the _interactions window_ of the DrRacket IDE, make the function call `(run style)`, replacing "style" with your preferred style (`original` or `classic`).
3. Once a game is begun, it can be stored by striking the `s` key.
4. A previously stored game may be loaded by striking the `l` key. You will need to select the stored-game file for loading.
5. Moves within a game may be undone by striking the `u` key.
