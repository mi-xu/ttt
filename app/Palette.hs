module Main where

import Brick.AttrMap (AttrName, attrName)
import Brick.AttrMap qualified as A
import Brick.Main qualified as M
import Brick.Types (BrickEvent (VtyEvent), EventM, Widget)
import Brick.Util (on)
import Brick.Widgets.Core (fill, padAll, vBox, withAttr)
import Graphics.Vty qualified as V

a1 :: AttrName
a1 = attrName "a1"

a2 :: AttrName
a2 = attrName "a2"

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (a1, V.black `on` V.red),
      (a2, V.black `on` V.green)
    ]

drawUI :: () -> [Widget ()]
drawUI _ =
  [ vBox
      [ withAttr a1 $ padAll 1 $ fill '@',
        withAttr a2 $ padAll 1 $ fill '@'
      ]
  ]

appEvent :: BrickEvent () e -> EventM () () ()
appEvent (VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> M.halt
    V.EvKey (V.KChar 'q') [] -> M.halt
    _ -> return ()
appEvent _ = return ()

app :: M.App () e ()
app =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.neverShowCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = return (),
      M.appAttrMap = const theMap
    }

main :: IO ()
main = do
  M.defaultMain app ()