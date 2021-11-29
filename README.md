# CSE 230 - Final Project

Members: Yuka Chu, Chi-Hsuan Lee, Yi-Ting Wang, Heidi Cheng

Link to Hackmd : https://hackmd.io/@heidiche/By63QCVvt

## Ping-Pong Game with a Twist
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
