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
import Control.Monad.IO.Class (MonadIO(liftIO))


-------------------------------------------------------------------------------
-- | Ball ---------------------------------------------------------------------
-------------------------------------------------------------------------------

data Coord = Coord
  { x :: Int 
  , y :: Int
  }
  deriving (Eq, Ord)

addc :: Coord -> Coord -> Coord
addc a b = Coord{ x = (x a) + (x b), y = (y a) + (y b)}

mulc :: Coord -> Float -> Coord
mulc a c = Coord{ x = round ((fromIntegral (x a)) * c), y = round ((fromIntegral (y a)) * c)}

data Ball   = Ball
  { pos   :: Coord -- ^ position of ball
  , dir   :: Coord -- ^ direction of ball moving towards
  , speed :: Float -- ^ speed * dir = actual move
  }

movement :: Ball -> Ball
movement b = do 
            let p = pos b
            let v = dir b
            let  s       = speed b
            b { pos = (p `addc` (v `mulc` s)) }

data Turn
  = P1
  | P2
  deriving (Eq, Show)

data Plane
  = X
  | Y
  deriving (Eq, Show)

reflect :: Ball -> Plane -> Ball
reflect b X = do 
    let v = dir b
    let vx' = -1 * (x v)
    b { dir = Coord {x = vx', y = y v} }

reflect b Y = do 
    let v = dir b
    let vy' = -1 * (y v)
    b { dir = Coord {x = x v, y = vy'} }


data Result a
  = Cont a
  | Hit Plane
  | Score Turn
  deriving (Eq, Functor, Show)

result :: Ball -> Racket -> Racket -> Result Ball -- ^ hit
result b p1 p2 = if (bx == 0) && (by > (p1+5)) || (by < (p1-5)) then Score P2
                else if (bx == 100) && (by > (p2+5)) || (by < (p2-5)) then Score P1
                     else if bx == 0 || bx == mx then Hit Y
                          else if by == 0 || by == my then Hit X
                               else Cont (movement b)
    where p   = pos b
          bx = x p
          by = y p

serveBall :: Turn -> Ball
serveBall P1 = Ball
  { pos   = Coord { x = mx `div`2, y = my `div` 2 }
  , dir   = Coord { x = -1, y = 0}
  , speed = 0.1
  }
serveBall P2 = Ball
  { pos   = Coord { x = mx `div`2, y = my `div` 2 }
  , dir   = Coord { x = 1, y = 0}
  , speed = 0.1
  }

isp = 0.1

initBall :: Ball
initBall = serveBall P1


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