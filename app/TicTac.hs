module Main where

import Brick.AttrMap qualified as A
import Brick.Main qualified as M
import Brick.Types (BrickEvent (VtyEvent), EventM, Widget)
import Brick.Util (fg)
import Brick.Widgets.Border qualified as B
import Brick.Widgets.Border.Style qualified as BS
import Brick.Widgets.Center qualified as C
import Brick.Widgets.Core (hBox, setAvailableSize, str, updateAttrMap, vBox, withAttr, withBorderStyle)
import Control.Monad.State (modify)
import Graphics.Vty qualified as V

data CellIndex = C0 | C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8
  deriving (Show, Eq, Ord)

left :: CellIndex -> CellIndex
left C1 = C0
left C2 = C1
left C4 = C3
left C5 = C4
left C7 = C6
left C8 = C7
left x = x

right :: CellIndex -> CellIndex
right C0 = C1
right C1 = C2
right C3 = C4
right C4 = C5
right C6 = C7
right C7 = C8
right x = x

down :: CellIndex -> CellIndex
down C0 = C3
down C1 = C4
down C2 = C5
down C3 = C6
down C4 = C7
down C5 = C8
down x = x

up :: CellIndex -> CellIndex
up C3 = C0
up C4 = C1
up C5 = C2
up C6 = C3
up C7 = C4
up C8 = C5
up x = x

data AppState where
  AppState :: {selectedCell :: CellIndex} -> AppState
  deriving (Show, Eq)

initialState :: AppState
initialState = AppState {selectedCell = C0}

cellContent :: A.AttrName
cellContent = A.attrName "cellContent"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr [(cellContent, fg V.black)]

withCellBorder :: Bool -> Widget () -> Widget ()
withCellBorder isSelected =
  updateAttrMap
    (A.applyAttrMappings [(B.borderAttr, fg (if isSelected then V.cyan else V.black))])

cell :: AppState -> CellIndex -> Widget ()
cell s i =
  setAvailableSize (9, 5)
    $ withCellBorder
      (selectedCell s == i)
    $ withBorderStyle BS.unicode
    $ withAttr cellContent
    $ B.border
    $ C.center
    $ str " "

drawUI :: AppState -> [Widget ()]
drawUI s =
  [ C.center $
      vBox
        [ hBox [c C0, c C1, c C2],
          hBox [c C3, c C4, c C5],
          hBox [c C6, c C7, c C8]
        ]
  ]
  where
    c = cell s

moveSelectedCell :: (CellIndex -> CellIndex) -> EventM () AppState ()
moveSelectedCell move = modify $ \s -> s {selectedCell = move (selectedCell s)}

appEvent :: BrickEvent () e -> EventM () AppState ()
appEvent (VtyEvent (V.EvKey V.KEsc [])) = M.halt
appEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = M.halt
appEvent (VtyEvent (V.EvKey (V.KChar 'h') [])) = moveSelectedCell left
appEvent (VtyEvent (V.EvKey (V.KChar 'l') [])) = moveSelectedCell right
appEvent (VtyEvent (V.EvKey (V.KChar 'j') [])) = moveSelectedCell down
appEvent (VtyEvent (V.EvKey (V.KChar 'k') [])) = moveSelectedCell up
appEvent _ = return ()

theApp :: M.App AppState e ()
theApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.neverShowCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = return (),
      M.appAttrMap = const theMap
    }

main :: IO ()
main = do
  d <- M.defaultMain theApp initialState
  print d
