{-# LANGUAGE RecordWildCards #-}
module Model where 

import Prelude hiding ((!!))
import qualified Model.Ball   as Ball
import qualified Model.Score  as Score
import qualified Model.Player as Player

-------------------------------------------------------------------------------
-- | Ticks mark passing of time: a custom event that we constantly stream
-------------------------------------------------------------------------------
data Tick = Tick

-------------------------------------------------------------------------------
-- | Top-level App State ------------------------------------------------------
-------------------------------------------------------------------------------

data State 
  = Intro 
  | Play PlayState 
  | Outro 
 
data Plane 
  = Racket
  | Wall

data PlayState = PS
  { racket1 :: Player.Racket   -- ^ racket on the left 
  , racket2 :: Player.Racket   -- ^ racket on the right
  , ball    :: Ball.Ball       -- ^ properties of the ball
  , result  :: Ball.Result     -- ^ game over flag
  , turn    :: Ball.Turn       -- ^ one of the player score, do nextServe. If end -> restart game
  , score   :: Score.Score     -- ^ score
  }

boardHeight, boardWidth, racketHeight :: Int
boardHeight  = 40
boardWidth   = 60
racketHeight = 5

init :: PlayState
init = PS
  { racket1 = 5                     
  , racket2 = 5                     
  , ball    = Ball.initBall              
  , result  = (Cont ball)           
  , turn    = P1                    
  , score   = (0, 0)                
  }


next :: PlayState -> Ball.Result Ball.Ball -> Either (Bool) PlayState
next s (Cont b') = Right (s { ball = b' } )
next s (Hit pl) = Right (s { ball = Ball.reflect (ball s) pl })
next s (Score p) = case (Score.addScore (score s) p) of
                         Left winner -> Left winner
                         Right ()    -> Right (s { ball = Ball.serveBall p })