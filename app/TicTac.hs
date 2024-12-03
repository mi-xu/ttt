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

data Mark = X | O
  deriving (Show, Eq)

nextState :: Maybe Mark -> Maybe Mark
nextState Nothing = Just X
nextState (Just X) = Just O
nextState (Just O) = Nothing

data AppState where
  AppState ::
    { selectedCell :: CellIndex,
      cellStates :: [Maybe Mark]
    } ->
    AppState
  deriving (Show, Eq)

type CellTriple = (CellIndex, CellIndex, CellIndex)

checkMatch :: CellTriple -> [Maybe Mark] -> Maybe (Mark, CellTriple)
checkMatch triple@(i1, i2, i3) states =
  let s1 = states !! fromEnum i1
      s2 = states !! fromEnum i2
      s3 = states !! fromEnum i3
   in case (s1, s2, s3) of
        (Just a, Just b, Just c) ->
          if a == b && b == c
            then Just (a, triple)
            else Nothing
        _ -> Nothing

checkWin :: [Maybe Mark] -> Maybe (Mark, CellTriple)
checkWin states =
  let matches =
        [ (C0, C1, C2),
          (C3, C4, C5),
          (C6, C7, C8),
          (C0, C3, C6),
          (C1, C4, C7),
          (C2, C5, C8),
          (C0, C4, C8),
          (C6, C4, C2)
        ]
      check [] = Nothing
      check (triple : rest) =
        case checkMatch triple states of
          Just result -> Just result
          Nothing -> check rest
   in check matches

initialState :: AppState
initialState = AppState {selectedCell = C0, cellStates = [Nothing | _ <- [C0 .. C8]]}

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

cell :: AppState -> Maybe (Mark, CellTriple) -> CellIndex -> Widget ()
cell s win i =
  let cellStyle = case win of
        Just (X, (i1, i2, i3)) ->
          if i == i1 || i == i2 || i == i3
            then winningX
            else cellContent
        Just (O, (i1, i2, i3)) ->
          if i == i1 || i == i2 || i == i3
            then winningO
            else cellContent
        _ -> cellContent
      label =
        ( case cellStates s !! fromEnum i of
            Nothing -> " "
            Just X -> "X"
            Just O -> "O"
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
resetCellState = modify $ \s -> s {cellStates = [Nothing | _ <- [C0 .. C8]]}

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
