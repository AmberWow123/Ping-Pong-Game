module Model.Player where

import Model

type Racket = Int  -- ^ position of racket
Player1 :: Racket
Player1 = boardHeight `div` 2
Player2 :: Racket
Player2 = boardHeight `div` 2