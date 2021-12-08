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

init :: IO PlayState
init = do{
   b <- Ball.init;
  return PS
  { racket1 = Player.player1                  
  , racket2 = Player.player2                     
  , ball    = b       
  , result  = Nothing          
  , turn    = P1                    
  , score   = (0, 0)                
  }
}


initsc :: Score -> Racket -> Racket -> IO PlayState
initsc sc p1 p2= do{
   b <- Ball.init;
  return PS
  { racket1 = p1                
  , racket2 = p2                     
  , ball    = b       
  , result  = Nothing          
  , turn    = P1                    
  , score   = sc                
  }
}

next :: PlayState -> Ball.Result Ball.Ball -> Either (Maybe Turn) (IO PlayState)
next s (Cont b') = Right (return (s { ball = b' } ))
next s (Hit pl) = Right (return (s { ball = Ball.movement (Ball.reflect (ball s) pl) }))
next s (Score p) = case (Score.addScore (score s) p) of
                         Left winner -> Left (Just winner)
                         Right sc -> Right (initsc sc (racket1 s) (racket2 s))
