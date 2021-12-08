module Model.Ball
  ( -- * Types
    Ball
  , Turn (..)
  , Plane (..)
  , Result (..)

    -- * Ball API
  , getIntCoord
  , movement
  , nextResult
  , init
  , reflect
  , serveBall
  

  )
  where

import Prelude hiding (init)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Types
import System.Random

-------------------------------------------------------------------------------
-- | Ball ---------------------------------------------------------------------
-------------------------------------------------------------------------------

addc :: Coord -> Coord -> Coord
addc a b = Coord{ x = (x a) + (x b), y = (y a) + (y b)}

mulc :: Coord -> Float -> Coord
mulc a c = Coord{ x = (x a) * c, y = (y a) * c}

getIntCoord :: Ball -> (Int,Int)
getIntCoord b = (round (x p), round (y p))
            where p = pos b

movement :: Ball -> Ball
movement b = do 
            let p = pos b
            let v = dir b
            let s = speed b
            b { pos = (p `addc` (v `mulc` s)) }

reflect :: Ball -> Plane -> Ball
reflect b X = do 
    let v = dir b
    let vx' = -1 * (x v)
    b { dir = Coord {x = vx', y = y v} }

reflect b Y = do 
    let v = dir b
    let vy' = -1 * (y v)
    b { dir = Coord {x = x v, y = vy'} }

nextResult :: Ball -> Racket -> Racket -> Result Ball -- ^ hit
nextResult b p1 p2 = if (bx == 5) && (by <= fromIntegral (p1+2)) && (by >= fromIntegral (p1-2)) then Hit X
                     else if (bx == fromIntegral boardWidth - 5) && (by <= fromIntegral (p2+2)) && (by >= fromIntegral (p2-2)) then Hit X
                          else if bx == 0 then Score P2
                               else if bx == fromIntegral boardWidth then Score P1
                                    else if by == 0 || by == fromIntegral boardHeight then Hit Y
                                         else Cont (movement b)
    where p   = pos b
          bx = x p
          by = y p

serveBall :: Turn -> Ball
serveBall P1 = Ball
  { pos   = Coord { x = fromIntegral (boardWidth `div`2), y = fromIntegral (boardHeight `div` 2) }
  , dir   = Coord { x = -1, y = 1}
  , speed = 1
  }
serveBall P2 = Ball
  { pos   = Coord { x = fromIntegral (boardWidth `div`2), y = fromIntegral (boardHeight `div` 2) }
  , dir   = Coord { x = 1, y = 1}
  , speed = 1
  }

init :: Ball
init = serveBall P1

serveRBall :: Turn -> IO Ball
serveRBall P1 = do
  i <- randomRIO(0,1)
  j <- randomRIO(-1,1)
  return Ball{ pos   = Coord { x = fromIntegral (boardWidth `div`2), y = fromIntegral (boardHeight `div` 2) }
             , dir   = Coord { x = i, y = j}
             , speed = 1
  }
serveRBall P2 = do
  i <- randomRIO(-1,0)
  j <- randomRIO(-1,1)
  return Ball{ pos   = Coord { x = fromIntegral (boardWidth `div`2), y = fromIntegral (boardHeight `div` 2) }
             , dir   = Coord { x = i, y = j}
             , speed = 1
  }

