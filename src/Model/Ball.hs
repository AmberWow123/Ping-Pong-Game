{-# LANGUAGE DeriveFunctor #-}
module Model.Ball
  ( -- * Types
    Ball
  , Turn (..)
  , Plane (..)
  , Result (..)

    -- * Ball API
  , result
  , next
  , initBall

  )
  where

import Prelude hiding (init)


-------------------------------------------------------------------------------
-- | Ball ---------------------------------------------------------------------
-------------------------------------------------------------------------------

type Coord  = (Int,Int)

data Ball   = Ball
  { pos   :: Coord -- ^ position of ball
  , dir   :: Coord -- ^ direction of ball moving towards
  , speed :: Float -- ^ speed * dir = actual move
  }

movement :: Ball -> Ball
movement b = b { pos = (x',y') }
           where (x,y)   = pos b
                 (vx,vy) = dir b
                 s       = speed b
                 x' = x + (round (s * fromIntegral vx))
                 y' = y + (round (s * fromIntegral vy))

data Turn
  = P1
  | P2
  deriving (Eq, Show)

data Plane
  = X
  | Y
  deriving (Eq, Show)

reflect :: Ball -> Plane -> Ball
reflect b X = b { dir = (vx',vy) }
             where (vx,vy) = dir b
                   vx' = -1 * vx
reflect b Y = b { dir = (vx,vy') }
             where (vx,vy) = dir b
                   vy' = -1 * vy


data Result a
  = Cont a
  | Hit Plane
  | Score Turn
  deriving (Eq, Functor, Show)

result :: Ball -> Racket -> Racket -> Result Ball -- ^ hit
result b p1 p2 = if (x == 0) && (y > (p1+5)) || (y < (p1-5)) then Score P2
                else if (x == 100) && (y > (p2+5)) || (y < (p2-5)) then Score P1
                     else if x == 0 || x == mx then Hit Y
                          else if y == 0 || y == my then Hit X
                               else Cont (movement b)
    where (x,y)   = pos b

randomBallDir :: Turn -> IO Coord
randomBallDir = undefined

serveBall :: Turn -> Ball
serveBall p = Ball {
  pos = (mx `div` 2, my `div` 2)
, dir = do{ d <- randomBallDir p; d }
, speed = isp
}

isp = 0.1

initBall :: Ball
initBall = undefined


--------------------------apis from other files
data PlayState = PS
  { racket1 :: Racket -- ^ racket on the left 
  , racket2 :: Racket -- ^ racket on the right
  , ball    :: Ball   -- ^ properties of the ball
  , result'  :: Turn  -- ^ game over flag
  , turn    :: Turn   -- ^ one of the player score, do nextServe. If end -> restart game
  , score   :: Score  -- ^ score
  }

type Result' = Turn
type Score = (Int, Int)
addScore :: Score -> Turn -> Either (Turn) () -- ^ Left winner, Right current Score after increment
addScore = undefined
type Racket = Int

mx :: Int
mx = 100
my:: Int
my = 60


---------- for Model.hs
next :: PlayState -> Result Ball -> Either (Turn) PlayState
next s (Cont b') = Right (s { ball = b' } )
next s (Hit pl) = Right (s { ball = reflect (ball s) pl })
next s (Score p) = case (addScore (score s) p) of
                         Left winner -> Left winner
                         Right ()    -> Right (s { ball = serveBall p })