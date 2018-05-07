module Main where

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data Team = R | Y deriving (Eq)

type Space = Maybe Team

data State = State {
    turn :: Team,
    board :: [[Space]]
}

opposingTeam :: Team -> Team
opposingTeam R = Y
opposingTeam Y = R

divf :: Int -> Int -> Float
divf a b = (fromIntegral a) / (fromIntegral b)

teamColor :: Team -> Color
teamColor R = red
teamColor Y = yellow

newBoard :: Int -> Int -> [[Space]]
newBoard columns rows = replicate rows (replicate columns Nothing)

bg :: Color
bg = black

frameColor :: Color
frameColor = blue

resolution :: (Int,Int)
resolution = (500,500)

spaceAt :: (Int,Int) -> State -> Space
spaceAt (col, row) state = ((board state)!!row)!!col

render :: State -> Picture
render state = do let cols = length (head (board state))
                  let rows = length (board state)
                  let colWidth = divf (fst resolution) cols
                  let rowHeight = divf (snd resolution) rows
                  let positions = [(x,y) | x <- [0..(cols-1)], y <- [0..(rows-1)]]
                  let cells = map (\(x,y) -> renderFrameCell (spaceAt (x,y) state) ((fromIntegral x) * colWidth,(fromIntegral y) * rowHeight) colWidth rowHeight) positions
                  Pictures cells

renderFrameCell :: Space -> (Float,Float) -> Float -> Float -> Picture
renderFrameCell space (xPos,yPos) width height = do let rect = Color frameColor (rectangleSolid width height)
                                                    let circ = renderSpace space width
                                                    let xOffset = xPos - (divf (fst resolution) 2) + (width / 2)
                                                    let yOffset = yPos - (divf (snd resolution) 2) + (height / 2)
                                                    Pictures (map (Translate xOffset yOffset) [rect,circ])

renderSpace :: Space -> Float -> Picture
renderSpace (Just team) width = Color (teamColor team) (circleSolid ((width / 2) - 4))
renderSpace Nothing width = Color bg (circleSolid ((width / 2) - 4))

changeSpace :: Int -> Int -> Space -> [[Space]] -> [[Space]]
changeSpace rowIndex columnIndex replacement spaces = do
    let (rowHead,row:rowTail) = splitAt rowIndex spaces
    let (colHead,col:colTail) = splitAt columnIndex row
    rowHead ++ (colHead ++ replacement : colTail) : rowTail

addPiece :: Team -> [[Space]] -> Int -> Int -> [[Space]]
addPiece team spaces column row | row < (length spaces) && (spaces!!row)!!column == Nothing = changeSpace row column (Just team) spaces
                                | row < (length spaces) = addPiece team spaces column (row + 1)
                                | otherwise = spaces

handleEvent :: Event -> State -> State
handleEvent (EventKey (MouseButton LeftButton) Down _ (x,_)) state = do
    let offsetX = x + (divf (fst resolution) 2)
    let selectedColumn = getClickedColumn offsetX (length (head (board state)))
    let updatedBoard = addPiece (turn state) (board state) selectedColumn 0
    state { board = updatedBoard, turn = opposingTeam (turn state) }
handleEvent _ state = state

getClickedColumn :: Float -> Int -> Int
getClickedColumn xPos numColumns = floor ((xPos / fromIntegral (fst resolution)) * fromIntegral numColumns)

checkRow :: Team -> [Space] -> Int -> Int -> Bool
checkRow team row longestRun currentRun | longestRun >= 4 = True
                                        | length row <= 0 = False
                                        | head row == Just team = checkRow team (tail row) (max (currentRun + 1) longestRun) (currentRun + 1)
                                        | otherwise = checkRow team (tail row) longestRun 0

checkCol :: Team -> [[Space]] -> Int -> Int -> Int -> Bool
checkCol team spaces colIndex longestRun currentRun | longestRun >= 4 = True
                                                    | length spaces <= 0 = False
                                                    | (head spaces)!!colIndex == Just team = checkCol team (tail spaces) colIndex (max (currentRun + 1) longestRun) (currentRun + 1)
                                                    | otherwise = checkCol team (tail spaces) colIndex longestRun 0

checkPosDiag :: Team -> [[Space]] -> Int -> Int -> Int -> Int -> Bool
checkPosDiag team spaces colIndex rowIndex longestRun currentRun | longestRun >= 4 = True
                                                              | colIndex >= length (head spaces) || rowIndex >= length spaces = False
                                                              | (spaces!!rowIndex)!!colIndex == Just team = do
                                                                  let newRun = currentRun + 1
                                                                  let newLongest = max newRun longestRun
                                                                  checkPosDiag team spaces (colIndex + 1) (rowIndex + 1) newLongest newRun
                                                              | otherwise = do
                                                                  checkPosDiag team spaces (colIndex + 1) (rowIndex + 1) longestRun 0

checkNegDiag :: Team -> [[Space]] -> Int -> Int -> Int -> Int -> Bool
checkNegDiag team spaces colIndex rowIndex longestRun currentRun | longestRun >= 4 = True
                                                              | colIndex >= length (head spaces) || rowIndex <= 0 = False
                                                              | (spaces!!rowIndex)!!colIndex == Just team = do
                                                                  let newRun = currentRun + 1
                                                                  let newLongest = max newRun longestRun
                                                                  checkNegDiag team spaces (colIndex + 1) (rowIndex - 1) newLongest newRun
                                                              | otherwise = do
                                                                  checkNegDiag team spaces (colIndex + 1) (rowIndex - 1) longestRun 0

isWin :: Team -> [[Space]] -> Bool
isWin team spaces = do let numRows = (length spaces)
                       let numCols = (length (head spaces))
                       let rowCombo = any id (map (\row -> checkRow team row 0 0) spaces)
                       let colCombo = any id (map (\col -> checkCol team spaces col 0 0) [0..numCols - 1])
                       let posDiagonals = [(0,row) | row <- [0..numRows - 1]] ++ [(col,0) | col <- [1..numCols-1]]
                       let posDiagCombo = any id (map (\(col,row) -> checkPosDiag team spaces col row 0 0) posDiagonals)
                       let negDiagonals = [(0,row) | row <- [0..numRows - 1]] ++ [(col,numRows-1) | col <- [1..numCols-1]]
                       let negDiagCombo = any id (map (\(col,row) -> checkNegDiag team spaces col row 0 0) negDiagonals)
                       rowCombo || colCombo || posDiagCombo || negDiagCombo

step :: Float -> State -> State
step time state | isWin (opposingTeam (turn state)) (board state) = state {board = [[Just (opposingTeam (turn state))]]}
                | otherwise = state

window :: Display
window = InWindow "Made with Haskell" resolution (0, 0)

run :: State -> IO()
run state |null (board state) = putStrLn "Empty Board"
          |null (head (board state)) = putStrLn "Empty Board"
          |otherwise = play window bg 120 state (render) (handleEvent) (step)

main :: IO()
main = do let state = State {turn = R, board = (newBoard 7 6)}
          run state
