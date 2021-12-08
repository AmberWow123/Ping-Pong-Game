{-# LANGUAGE DeriveFunctor #-}
module Types where

-------------------------------------------------------------------------------
-- | Ticks mark passing of time: a custom event that we constantly stream
-------------------------------------------------------------------------------
data Tick = Tick

-------------------------------------------------------------------------------
-- | Data for View
-------------------------------------------------------------------------------
data HitPlane 
  = Racket
  | Wall

-------------------------------------------------------------------------------
-- | Data for Ball
-------------------------------------------------------------------------------
data Coord = Coord
  { x :: Float 
  , y :: Float
  }
  deriving (Eq, Ord)

data Turn
  = P1
  | P2
  deriving (Eq, Show)

data Plane
  = X
  | Y
  deriving (Eq, Show)

data Ball   = Ball
  { pos   :: Coord -- ^ position of ball
  , dir   :: Coord -- ^ direction of ball moving towards
  , speed :: Float -- ^ speed * dir = actual move
  }

data Result a
  = Cont a
  | Hit Plane
  | Score Turn
  deriving (Eq, Functor, Show)

-------------------------------------------------------------------------------
-- | Data for Player
-------------------------------------------------------------------------------
type Racket = Int  -- ^ position of racket

-------------------------------------------------------------------------------
-- | Data for Player
-------------------------------------------------------------------------------
type Score = (Int, Int)


boardHeight, boardWidth, racketHeight :: Int
boardHeight  = 40
boardWidth   = 60
racketHeight = 5