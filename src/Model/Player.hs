module Model.Player where

import Types

type Racket = Int  -- ^ position of racket

player1 :: Racket
player1 = boardHeight `div` 2
player2 :: Racket
player2 = boardHeight `div` 2