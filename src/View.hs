import qualified Brick.Widgets.Border.Style as BS
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
  $ vBox [ drawScore (g ^. score)
         , padTop (Pad 1) $ drawGameOver (g ^. result)
         ]

drawScore :: (Int, Int) -> Widget n
drawScore (p1_score, p2_score) = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ (str "Player1 score") $ show p1_score
  $ padRight 1
  $ str $ (str "Player2 score") $ show p2_score

drawGameOver :: Bool -> Widget n
drawGameOver dead =
  if dead
     then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
     else emptyWidget

gameOverAttr :: AttrName
gameOverAttr = "gameOver"