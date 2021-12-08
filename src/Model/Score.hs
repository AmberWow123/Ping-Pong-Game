module Model.Score where

import Types
import Model.Ball

addScore :: Score -> Turn -> Either (Bool) (Score) -- ^ Left winner, Right current Score after increment
addScore s t = let (s1,s2) = s in case t of
                                        P1 -> if s1 +1 > 11 then Left True
                                              else Right (s1+1,s2)
                                        P2 -> if s2 +1 > 11 then Left False
                                              else Right (s1,s2+1)