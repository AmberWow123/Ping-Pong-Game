module Model.Ball
  ( -- * Types
    Ball
  , Turn (..)
  , Plane (..)
  , Result (..)

    -- * Ball API
  , getIntPos
  , result
  , init

  )
  where

import Prelude hiding (init)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Types
import Model.Player

-------------------------------------------------------------------------------
-- | Ball ---------------------------------------------------------------------
-------------------------------------------------------------------------------

addc :: Coord -> Coord -> Coord
addc a b = Coord{ x = (x a) + (x b), y = (y a) + (y b)}

mulc :: Coord -> Float -> Coord
mulc a c = Coord{ x = (x a) * c, y = (y a) * c}

getIntPos :: Ball -> (Int,Int)
getIntPos b = (round (x p), round (y p))
            where p = pos b

movement :: Ball -> Ball
movement b = do 
            let p = pos b
            let v = dir b
            let  s       = speed b
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

result :: Ball -> Racket -> Racket -> Result Ball -- ^ hit
result b p1 p2 = if (bx == 0) && (by > (p1+5)) || (by < (p1-5)) then Score P2
                else if (bx == boardWidth) && (by > (p2+5)) || (by < (p2-5)) then Score P1
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