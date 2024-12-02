module Main where

import Brick.AttrMap qualified as A
import Brick.Main qualified as M
import Brick.Types (BrickEvent (VtyEvent), EventM, Widget)
import Brick.Util (fg, on)
import Brick.Widgets.Border qualified as B
import Brick.Widgets.Border.Style qualified as BS
import Brick.Widgets.Center qualified as C
import Brick.Widgets.Core (hBox, setAvailableSize, str, updateAttrMap, vBox, withAttr, withBorderStyle)
import Control.Monad.State (modify)
import Graphics.Vty qualified as V

data CellIndex = C0 | C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8
  deriving (Show, Eq, Ord, Enum)

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

data CellState = Empty | X | O
  deriving (Show, Eq)

nextState :: CellState -> CellState
nextState Empty = X
nextState X = O
nextState O = Empty

data AppState where
  AppState ::
    { selectedCell :: CellIndex,
      cellStates :: [CellState]
    } ->
    AppState
  deriving (Show, Eq)

type CellTriple = (CellIndex, CellIndex, CellIndex)

checkMatch :: CellTriple -> [CellState] -> CellState
checkMatch (i1, i2, i3) states =
  let s1 = states !! fromEnum i1
      s2 = states !! fromEnum i2
      s3 = states !! fromEnum i3
   in if s1 == s2 && s2 == s3 && s1 /= Empty then s1 else Empty

checkWin :: [CellState] -> (CellState, Maybe CellTriple)
checkWin states = go matches
  where
    matches =
      [ (C0, C1, C2),
        (C3, C4, C5),
        (C6, C7, C8),
        (C0, C3, C6),
        (C1, C4, C7),
        (C2, C5, C8),
        (C0, C4, C8),
        (C6, C4, C2)
      ]
    go [] = (Empty, Nothing)
    go (triple : rest) =
      let result = checkMatch triple states
       in if result /= Empty
            then (result, Just triple)
            else go rest

initialState :: AppState
initialState = AppState {selectedCell = C0, cellStates = [Empty | _ <- [C0 .. C8]]}

cellContent :: A.AttrName
cellContent = A.attrName "cellContent"

winningX :: A.AttrName
winningX = A.attrName "winningX"

winningO :: A.AttrName
winningO = A.attrName "winningO"

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (cellContent, fg V.black),
      (winningX, fg V.blue),
      (winningO, fg V.red)
    ]

withCellBorder :: Bool -> Widget () -> Widget ()
withCellBorder isSelected =
  updateAttrMap
    (A.applyAttrMappings [(B.borderAttr, fg (if isSelected then V.cyan else V.black))])

cell :: AppState -> (CellState, Maybe CellTriple) -> CellIndex -> Widget ()
cell s (_, wt) i =
  let isWinner = case wt of
        Just (i1, i2, i3) -> i == i1 || i == i2 || i == i3
        Nothing -> False
      cellStyle =
        ( case (isWinner, cellStates s !! fromEnum i) of
            (True, X) -> winningX
            (True, O) -> winningO
            _ -> cellContent
        )
      label =
        ( case cellStates s !! fromEnum i of
            Empty -> " "
            X -> "X"
            O -> "O"
        )
   in setAvailableSize (9, 5)
        $ withCellBorder
          (selectedCell s == i)
        $ withBorderStyle BS.unicode
        $ withAttr cellStyle
        $ B.border
        $ C.center
        $ str label

drawUI :: AppState -> [Widget ()]
drawUI s =
  let winner = checkWin $ cellStates s
      c = cell s winner
   in [ C.center $
          vBox
            [ hBox [c C0, c C1, c C2],
              hBox [c C3, c C4, c C5],
              hBox [c C6, c C7, c C8]
            ]
      ]

moveSelectedCell :: (CellIndex -> CellIndex) -> EventM () AppState ()
moveSelectedCell move = modify $ \s -> s {selectedCell = move (selectedCell s)}

cycleSelectedCellState :: EventM () AppState ()
cycleSelectedCellState = modify $ \s ->
  s
    { cellStates =
        zipWith
          (\i cs -> if i == selectedCell s then nextState cs else cs)
          [C0 .. C8]
          (cellStates s)
    }

resetCellState :: EventM () AppState ()
resetCellState = modify $ \s -> s {cellStates = [Empty | _ <- [C0 .. C8]]}

appEvent :: BrickEvent () e -> EventM () AppState ()
appEvent (VtyEvent (V.EvKey V.KEsc [])) = M.halt
appEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = M.halt
appEvent (VtyEvent (V.EvKey (V.KChar 'h') [])) = moveSelectedCell left
appEvent (VtyEvent (V.EvKey (V.KChar 'l') [])) = moveSelectedCell right
appEvent (VtyEvent (V.EvKey (V.KChar 'j') [])) = moveSelectedCell down
appEvent (VtyEvent (V.EvKey (V.KChar 'k') [])) = moveSelectedCell up
appEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) = cycleSelectedCellState
appEvent (VtyEvent (V.EvKey (V.KChar 'r') [])) = resetCellState
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
