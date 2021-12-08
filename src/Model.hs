{-# LANGUAGE RecordWildCards #-}

module Model where 

import Prelude hiding ((!!))
import Types
import qualified Model.Ball   as Ball
import qualified Model.Score  as Score
import qualified Model.Player as Player

-------------------------------------------------------------------------------
-- | Top-level App State ------------------------------------------------------
-------------------------------------------------------------------------------

init :: PlayState
init = PS
  { racket1 = Player1                  
  , racket2 = Player2                     
  , ball    = Ball.init            
  , result  = (Cont ball)           
  , turn    = P1                    
  , score   = (0, 0)                
  }

next :: PlayState -> Ball.Result Ball.Ball -> Either (Bool) PlayState
next s (Cont b') = Right (s { ball = b' } )
next s (Hit pl) = Right (s { ball = Ball.reflect (ball s) pl })
next s (Score p) = case (Score.addScore (score s) p) of
                         Left winner -> Left winner
                         Right sc -> Right (s { ball = Ball.serveBall p, score = sc })
