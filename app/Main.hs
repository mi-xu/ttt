module Main where

-- import Brick

-- import Brick.Widgets.Border (borderWithLabel, vBorder)
-- import Brick.Widgets.Border.Style (unicode)
-- import Brick.Widgets.Center (center)

import Brick.AttrMap qualified as A
import Brick.Main qualified as M
import Brick.Types (BrickEvent (..), EventM, Widget)
import Brick.Util (bg, on)
import Brick.Widgets.Center qualified as C
import Brick.Widgets.Core (padAll, str)
import Brick.Widgets.Dialog qualified as D
import Graphics.Vty qualified as V

data Choice = Red | Green | Blue
  deriving (Show)

data Name = RedButton | BlueButton | GreenButton
  deriving (Show, Eq, Ord)

type S = D.Dialog Choice Name

drawUI :: S -> [Widget Name]
drawUI d = [ui]
  where
    ui = D.renderDialog d $ C.hCenter $ padAll 1 $ str "Pick one"

initialState :: S
initialState =
  D.dialog (Just $ str "Color") (Just (RedButton, choices)) 50
  where
    choices =
      [ ("Red", RedButton, Red),
        ("Blue", BlueButton, Blue),
        ("Green", GreenButton, Green)
      ]

appEvent :: BrickEvent Name e -> EventM Name S ()
appEvent (VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> M.halt
    V.EvKey V.KEnter [] -> M.halt
    _ -> D.handleDialogEvent ev
appEvent _ = return ()

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (D.dialogAttr, V.white `on` V.black),
      (D.buttonAttr, V.black `on` V.white),
      (D.buttonSelectedAttr, bg V.cyan)
    ]

theApp :: M.App S e Name
theApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = return (),
      M.appAttrMap = const theMap
    }

main :: IO ()
main = do
  d <- M.defaultMain theApp initialState
  putStrLn $
    "You chose: " <> maybe "Nothing" (show . snd) (D.dialogSelection d)
