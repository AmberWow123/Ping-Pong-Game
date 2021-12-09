module Main where

import Brick
import Graphics.Vty.Attributes
import qualified Graphics.Vty as V
import Brick.BChan (newBChan, writeBChan)
import Control.Monad (forever)
import Control.Concurrent (threadDelay, forkIO)

import Types
import Model
import View 
import Control 
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

-------------------------------------------------------------------------------
main :: IO ()
main = do
  chan   <- newBChan 10
  forkIO  $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- decides how fast your game moves
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  modelInit <- Model.init
  res <- customMain initialVty buildVty (Just chan) app modelInit
  print (result res, score res) 

app :: App PlayState Tick Name
app = App
  { appDraw         = drawUI 
  , appChooseCursor = const . const Nothing
  , appHandleEvent  = control 
  , appStartEvent   = return
  , appAttrMap      = const theMap    -- (attrMap defAttr [])
  }