module View(view) where

import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  , (<=>)
  )

drawUI :: PlayState -> [Widget n]
drawUI g = [C.center $ padTop (Pad 2) (drawStats g) <=> drawBoard g]

drawStats :: PlayState -> Widget n
drawStats g = hLimit 5
  $ vBox [ drawScore (score g)
         , padTop (Pad 1) $ drawGameOver (result g)
         ]

drawScore :: (Int, Int) -> Widget n
drawScore (p1_score, p2_score) = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ (str "Player1 score") $ show p1_score
  $ padRight 1
  $ str $ (str "Player2 score") $ show p2_score

drawGameOver :: (Int, Int) -> Widget n
drawGameOver (p1_result, p2_result) =
  if (p1_result + p2_result) > 0
      if p1_result == 1:
          then withAttr gameOverAttr $ C.hCenter $ str "PLAYER 1 WON"
          else withAttr gameOverAttr $ C.hCenter $ str "PLAYER 2 WON"

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

