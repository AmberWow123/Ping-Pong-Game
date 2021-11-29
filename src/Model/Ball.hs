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
import Model
import Model.Player


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
                     else if bx == 0 || bx == boardWidth then Hit Y
                          else if by == 0 || by == boardHeight then Hit X
                               else Cont (movement b)
    where p   = pos b
          bx = x p
          by = y p

serveBall :: Turn -> Ball
serveBall P1 = Ball
  { pos   = Coord { x = boardWidth `div`2, y = boardHeight `div` 2 }
  , dir   = Coord { x = -1, y = 0}
  , speed = 0.1
  }
serveBall P2 = Ball
  { pos   = Coord { x = boardWidth `div`2, y = boardHeight `div` 2 }
  , dir   = Coord { x = 1, y = 0}
  , speed = 0.1
  }

isp = 0.1

init :: Ball
init = serveBall P1