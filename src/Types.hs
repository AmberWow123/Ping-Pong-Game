module Types where

-------------------------------------------------------------------------------
-- | Ticks mark passing of time: a custom event that we constantly stream
-------------------------------------------------------------------------------
data Tick = Tick

data HitPlane 
  = Racket
  | Wall

boardHeight, boardWidth, racketHeight :: Int
boardHeight  = 40
boardWidth   = 60
racketHeight = 5