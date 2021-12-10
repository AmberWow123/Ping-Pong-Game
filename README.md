# CSE 230 - Final Project

Members: Yuka Chu, Chi-Hsuan Lee, Yi-Ting Wang, Heidi Cheng

Link to Hackmd : https://hackmd.io/@heidiche/By63QCVvt

## Ping-Pong Game with a Twist
![](https://i.imgur.com/o0QwnmI.gif)
### Rules
* Two-player ping pong game
* Control vertical position of two rackets with keyboard (`w` and `s` for Player 1; `up` and `down` for Player 2)
* One earns a point when the other player misses the ball
* The next ball is served towards the previous scored player
* The second ball is added after someone gets 3 points
* Game ends when one of the players hit a score of 5

### Features 
* Randomly generate ball's initial direction
* Ball reflects when hitting the rackets, ceiling, and floor
* Determine a player earns points when balls hit the opponent's sidewall 
* Enable two players to controls the rackets
* Add an extra ball to increase difficulty in the pong game


### Program Architecture
Libraries: [brick](https://hackage.haskell.org/package/brick), [vty](https://hackage.haskell.org/package/vty)
References: [ranjitjhala/brick-tac-toe](https://github.com/ranjitjhala/brick-tac-toe), [samtay/snake](https://github.com/samtay/snake)

```
src
 ├── Model 
 │   ├── Ball.hs
 │   ├── Player.hs
 │   └── Score.hs
 ├── Control.hs 
 ├── Main.hs
 ├── Model.hs
 ├── Types.hs
 └── View.hs
```
`Ball.hs` controls the movement of the ball.
`Player.hs` records the position of the rackets.
`Score.hs` is responsible for adding scores for players and determining if there is a winner.
`Control.hs` transforms states when there are keyboard interrupt, etc.
`Main.hs` contains the main function defined by the `brick` package.
`Model.hs` wraps up the interaction between files under the directory, `Model`.
`Types.hs` stores all of the data types we define.
`View.hs` creates the UI of the program.


### Difficulties and Solutions
1. Assemble each part of work from our group members and make it execute correctly
   - We designed clear APIs (data types) to let each group member commit individually without worrying about conflicts
2. To randomly serve balls, we had to deal with IO
   - We made the output of serving a ball a IO monad (Since generating a random number in Haskell is a IO)
3. We needed to deal with the movement of two balls separately and defined when to consider the second ball
   - In our PlayState, there are two objects: ball1 and ball2, each of them is a data - Ball.
   - We used an extra Boolean variable in PlayState to record whether the second ball is added to the game.

### Limitations
1. Due to the nature of pixel games, it is inevitable that the ball moves discretely
![](https://i.imgur.com/J26NxxX.png)

2. Both players have to press and release the keyboard to move.
   (when two players press to move at the same time, one player gets stuck)
## Milestone 1: Proposal
<p>
In this project, we are developing a two-player ping-pong game with additional feature such as smashing or spinning the ball.
    
As a normal ping-pong game, both players start with zero point, the first player reaching eleven point wins the game. Players serve two serves each, alternating. And if a game ties at 10-10, a player must win by 2 points. In this situation, players serve one serve each, alternating.
    
Each player can control a ping pong racket with keyboard keys. They are targeting to catch the ball and return it with the racket. If one fails to catch the ball, the other player scores.
    
Strategies below increase the chance to score:
- Smashing a ball means, in some circumstances, you can hit the ball harder to increase its speed. This gives your oppenent less time to react, and therefore more difficult to hit the ball back to you.

- Spinning the ball is to let the ball not go in a straight line, but in a curve. This makes the landing position harder to predict.
</p>

![](https://i.imgur.com/ogMj5VT.png)
![](https://i.imgur.com/okeCoQP.jpg)

## Milestone 2: Updates
* What is the architecture of your application (the key components)?
We mapped our functionalities with the ones in the starter-code (brick-tac-toe: https://github.com/ranjitjhala/brick-tac-toe), and modified the architecture of it.
```
src
 ├── Model 
 │   ├── Ball.hs
 │   ├── Player.hs
 │   └── Score.hs
 ├── Control.hs 
 ├── Main.hs
 ├── Model.hs
 └── View.hs
```
* What challenges (if any) did you have so far and how did you solve them?
1. Our program's structure is similar to the exsiting Haskell/brick TUI game "snake" (https://github.com/jtdaugherty/brick/blob/master/docs/samtay-tutorial.md). However, when we try to install and execute the snake game, some error occured (unable to download the depending packages) and we can't run it. This make us difficult to build our program using the snake as a template. We ended up modifying the brick-tack-toe.
2. We divided our work into four components so that each group member could commit individually without worrying about conflicts. To do so, we first design clear APIs to let each component communicate. We have not run our code yet. We are not sure if each component will connect well with the expected features.

* Do you expect to meet your goals until the deadline?

We were planning to add some additional features, such as smashing and spinning the ball. However, since we have three moving objects, it is hard for us to control the three objects. Therefore, we will focus on the implementation of ball's movement and two rackets for now.
* If not, how will you modify your goals?
1. We will add some addtional UI features, like changing the color of the direction arrow pressed by the player. 
2. We will try to implement spinning and smashing the ball after finishing the above modified goal. 

### Distribution of work

| Member |  |
| -------- | -------- |
| Yuka Chu    | Player.hs + Ball.hs   |
| Yi-Ting Wang     | Score.hs + Control.hs   |
| Chi-Hsuan Lee | View.hs |
| Heidi Cheng | Model.hs + Main.hs | 
