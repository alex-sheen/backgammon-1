# Backgammon: CMSC 15100 Final Project
The final project for UChicago's Intro to Computer Science I course: multiplayer backgammon with support for undoing moves and storing/loading games through serialization—converts between structured data and linear string representations of structured data. Makes ample use of recursion and linked lists. Developed in two styles, [**Original**](#Original) and [**Classic**](#Classic), as pictured below.

## Original
```
(run original)
```
![Initial game, Original style](images/initial.png)
![Final game, Original style](images/final.png)

## Classic
```
(run classic)
```
![Classic style](images/classic-style.png)

## Getting Started
These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. Have fun!

### Prerequisites
Download the `[DrRacket]`(https://download.racket-lang.org) IDE.

### Run
1. Clone this repo into your folder of choice.
    ```
    $ git clone https://github.com/jackandthebean/backgammon.git
    ```
2. In the `project3` folder, open the `project3-revised.rkt` file in `DrRacket` and click **Run**.
3. In the `DrRacket` _interactions window_, make the function call `(run style)`, replacing "style" with your preferred style (`original` or `classic`).

### Play
* Once a game is begun, it can be stored by striking the `s` key.
* A previously stored game may be loaded by striking the `l` key. You will need to select a stored-game file for loading. The `project3` folder includes a sample `saved-game` file that can be loaded.
* Moves within a game may be undone by striking the `u` key.

## Built With
* [DrRacket](https://download.racket-lang.org) – Racket language IDE
