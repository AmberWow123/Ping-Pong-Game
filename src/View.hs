module View where

import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Types
import Brick(
  AttrMap, Widget, hLimit, vBox, hBox, padTop, padAll, Padding(..) ,withBorderStyle,
  str, attrMap, withAttr, emptyWidget, AttrName, on, fg, (<=>), attrName
  )

drawUI :: PlayState -> [Widget Name]
drawUI g = [C.center $ padTop (Pad 2) (drawStats g) <=> drawBoard g]

drawStats :: PlayState -> Widget Name
drawStats g = hLimit 20
  $ vBox [ drawScore (score g)
         , padTop (Pad 1) $ drawGameOver (result g)
         ]

drawScore :: (Int, Int) -> Widget Name
drawScore (p1_score, p2_score) = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show (p1_score, p2_score)

drawGameOver :: Maybe Turn -> Widget Name
drawGameOver Nothing = emptyWidget
drawGameOver (Just P1) = withAttr gameOverAttr $ C.hCenter $ str "PLAYER 1 WON"
drawGameOver (Just P2) = withAttr gameOverAttr $ C.hCenter $ str "PLAYER 2 WON"

gameOverAttr :: AttrName
gameOverAttr = attrName "gameOver"

-- ==================================================================

drawBoard :: PlayState -> Widget Name
drawBoard g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Game")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [boardHeight-1, boardHeight-2..0]]
    cellsInRow y = [drawCoord (x, y) | x <- [0..boardWidth-1]]
    drawCoord    = drawCell . cellAt
    cellAt (a, b)
      | Coord {x=fromIntegral a, y=fromIntegral b} == Coord {x=5.0, y=fromIntegral (racket1 g)}        = Racket
      | Coord {x=fromIntegral a, y=fromIntegral b} == Coord {x=5.0, y=fromIntegral (racket1 g) + 1.0}  = Racket
      | Coord {x=fromIntegral a, y=fromIntegral b} == Coord {x=5.0, y=fromIntegral (racket1 g) - 1.0}  = Racket
      | Coord {x=fromIntegral a, y=fromIntegral b} == Coord {x=5.0, y=fromIntegral (racket1 g) + 2.0}  = Racket
      | Coord {x=fromIntegral a, y=fromIntegral b} == Coord {x=5.0, y=fromIntegral (racket1 g) - 2.0}  = Racket
      | Coord {x=fromIntegral a, y=fromIntegral b} == Coord {x=55.0, y=fromIntegral (racket2 g)}       = Racket
      | Coord {x=fromIntegral a, y=fromIntegral b} == Coord {x=55.0, y=fromIntegral (racket2 g) + 1.0} = Racket
      | Coord {x=fromIntegral a, y=fromIntegral b} == Coord {x=55.0, y=fromIntegral (racket2 g) - 1.0} = Racket
      | Coord {x=fromIntegral a, y=fromIntegral b} == Coord {x=55.0, y=fromIntegral (racket2 g) + 2.0} = Racket
      | Coord {x=fromIntegral a, y=fromIntegral b} == Coord {x=55.0, y=fromIntegral (racket2 g) - 2.0} = Racket
      | Coord {x=fromIntegral a, y=fromIntegral b} == pos (ball g)                                     = ViewBall
      | otherwise         = Empty

drawCell :: HitPlane -> Widget Name
drawCell Racket     = withAttr racketAttr cw
drawCell ViewBall   = withAttr ballAttr cb
drawCell Empty      = withAttr emptyAttr cw

cw :: Widget Name
cw = str " "

cb :: Widget Name
cb = str " "

racketAttr, ballAttr, emptyAttr :: AttrName
racketAttr = attrName "racketAttr"
ballAttr   = attrName "ballAttr"
emptyAttr  = attrName "emptyAttr"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (racketAttr, V.blue `on` V.blue)
  , (ballAttr, V.red `on` V.red)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  ]