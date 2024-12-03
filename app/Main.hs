module Main where

import Brick.AttrMap qualified as A
import Brick.Main qualified as M
import Brick.Types (BrickEvent (VtyEvent), EventM, Widget)
import Brick.Util (fg)
import Brick.Widgets.Border qualified as B
import Brick.Widgets.Border.Style qualified as BS
import Brick.Widgets.Center qualified as C
import Brick.Widgets.Core
  ( hBox,
    setAvailableSize,
    str,
    updateAttrMap,
    vBox,
    vLimit,
    withAttr,
    withBorderStyle,
  )
import Control.Monad.State (modify)
import Data.Maybe (isJust, isNothing)
import Graphics.Vty qualified as V

-- Types
data CellIndex = C0 | C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8
  deriving (Show, Eq, Ord, Enum)

data AppState where
  AppState ::
    { selectedCell :: CellIndex,
      cellStates :: [Maybe Mark]
    } ->
    AppState
  deriving (Show, Eq)

type CellTriple = (CellIndex, CellIndex, CellIndex)

data Mark = X | O
  deriving (Show, Eq)

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

matches :: [CellTriple]
matches =
  [ -- horizontal
    (C0, C1, C2),
    (C3, C4, C5),
    (C6, C7, C8),
    -- vertical
    (C0, C3, C6),
    (C1, C4, C7),
    (C2, C5, C8),
    -- diagonal
    (C0, C4, C8),
    (C6, C4, C2)
  ]

inTriple :: CellIndex -> CellTriple -> Bool
inTriple i (a, b, c) = i `elem` [a, b, c]

-- Styling
baseCellStyle :: A.AttrName
baseCellStyle = A.attrName "baseCellStyle"

winStyle :: Mark -> A.AttrName
winStyle X = A.attrName "winningX"
winStyle O = A.attrName "winningO"

nextMarkStyle :: A.AttrName
nextMarkStyle = A.attrName "nextMarkStyle"

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (baseCellStyle, fg V.black),
      (winStyle X, fg V.blue),
      (winStyle O, fg V.red),
      (nextMarkStyle, fg V.cyan)
    ]

withCellBorder :: Bool -> Widget () -> Widget ()
withCellBorder isActive =
  updateAttrMap
    (A.applyAttrMappings [(B.borderAttr, fg (if isActive then V.cyan else V.black))])

-- Logic
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
checkWin ms =
  let check [] = Nothing
      check (triple : rest) =
        case checkMatch triple ms of
          Just result -> Just result
          Nothing -> check rest
   in check matches

nextMark :: [Maybe Mark] -> Mark
nextMark ms =
  let count mark = length $ filter (== Just mark) ms
   in if count O < count X then O else X

checkFull :: [Maybe Mark] -> Bool
checkFull = all isJust

checkDone :: [Maybe Mark] -> Bool
checkDone ms = isJust (checkWin ms) || checkFull ms

-- Rendering
cell :: Bool -> Bool -> Bool -> Maybe Mark -> Mark -> Widget ()
cell isDisabled isInWin isSelected mark mark' =
  let borderStyle =
        if isSelected && not isDisabled
          then fg V.cyan
          else fg V.black
      cellStyle = case (isInWin, isDisabled, isSelected, mark) of
        (True, _, _, Just m) -> winStyle m
        (_, False, True, Nothing) -> nextMarkStyle
        _ -> baseCellStyle
      label = case (mark, isSelected) of
        (Just m, _) -> show m
        (Nothing, True) -> show mark'
        (Nothing, False) -> " "
   in setAvailableSize
        (9, 5)
        $ updateAttrMap (A.applyAttrMappings [(B.borderAttr, borderStyle)])
        $ withBorderStyle BS.unicode
        $ withAttr cellStyle
        $ B.border
        $ C.center
        $ str label

drawUI :: AppState -> [Widget ()]
drawUI s =
  let ms = cellStates s
      win = checkWin ms
      isFull = checkFull ms
      label = vLimit 1 . C.hCenter . str
      c i =
        cell
          isFull
          (maybe False (\(_, t) -> inTriple i t) win)
          (i == selectedCell s)
          (ms !! fromEnum i)
          (nextMark ms)
      title = case (win, isFull) of
        (Nothing, False) -> "Tic Tac Toe"
        (Nothing, True) -> "It's a tie!"
        (Just (m, _), _) -> show m <> " wins!"
      instructions = case (win, isFull) of
        (Nothing, False) -> "h,j,k,l → move  ␣ → pick  q → quit"
        _ -> "␣ → reset  q → quit"
   in [ C.center $
          vBox
            [ label title,
              C.hCenter $ hBox [c C0, c C1, c C2],
              C.hCenter $ hBox [c C3, c C4, c C5],
              C.hCenter $ hBox [c C6, c C7, c C8],
              label instructions
            ]
      ]

-- Event Handling
move :: (CellIndex -> CellIndex) -> EventM () AppState ()
move dir = modify $ \s ->
  if checkDone (cellStates s)
    then s
    else s {selectedCell = dir (selectedCell s)}

pick :: EventM () AppState ()
pick = modify $ \s ->
  let ms = cellStates s
   in if checkDone ms
        then
          initialState
        else
          s
            { cellStates =
                [ if i == selectedCell s && isNothing c
                    then Just (nextMark ms)
                    else c
                  | (i, c) <- zip [C0 .. C8] ms
                ]
            }

appEvent :: BrickEvent () e -> EventM () AppState ()
appEvent (VtyEvent (V.EvKey (V.KChar 'h') [])) = move left
appEvent (VtyEvent (V.EvKey (V.KChar 'l') [])) = move right
appEvent (VtyEvent (V.EvKey (V.KChar 'j') [])) = move down
appEvent (VtyEvent (V.EvKey (V.KChar 'k') [])) = move up
appEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) = pick
appEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = M.halt
appEvent (VtyEvent (V.EvKey V.KEsc [])) = M.halt
appEvent _ = return ()

-- Main
initialState :: AppState
initialState =
  AppState
    { selectedCell = C4,
      cellStates = [Nothing | _ <- [C0 .. C8]]
    }

main :: IO ()
main = do
  d <-
    M.defaultMain
      M.App
        { M.appDraw = drawUI,
          M.appChooseCursor = M.neverShowCursor,
          M.appHandleEvent = appEvent,
          M.appStartEvent = return (),
          M.appAttrMap = const theMap
        }
      initialState
  print d
