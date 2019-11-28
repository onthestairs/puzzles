module Puzzles.TrainTracks where

import qualified Data.Map.Strict as M
import Effects.Shuffle
import Polysemy
import Prelude hiding (Down, Left, Right)

type Grid = [[Int]]

type Position = (Int, Int)

data Piece = UpLeft | UpRight | DownLeft | DownRight | Horizontal | Vertical deriving (Show, Eq)

data Direction = Up | Down | Left | Right deriving (Show, Eq)

data PathPiece
  = PathPiece
      { piece :: !Piece,
        position :: !Position,
        outDirection :: !Direction
      }
  deriving (Show, Eq)

allPieces = [UpLeft, UpRight, DownLeft, DownRight, Horizontal, Vertical]

makePaths :: (Member Shuffle r) => Int -> Int -> Sem r [[PathPiece]]
makePaths rows cols = do
  -- startingPosition <- pickStartingPosition rows cols
  -- startingPiece <- pickStartingPiece
  let startingPosition = (0, 0)
  let startingPiece = UpRight
  let startingOutDirection = Right
  -- let startingDirection = getStartingDirection startingPosition startingPiece
  let startingPathPiece = PathPiece {piece = startingPiece, position = startingPosition, outDirection = startingOutDirection}
  findPaths rows cols (startingPathPiece :| [])

getNextDirection Down UpLeft = Left
getNextDirection Down UpRight = Right
getNextDirection Up DownLeft = Left
getNextDirection Up DownRight = Right
getNextDirection Left UpRight = Up
getNextDirection Left DownRight = Down
getNextDirection Right UpLeft = Up
getNextDirection Right DownLeft = Down
getNextDirection Down Vertical = Down
getNextDirection Up Vertical = Up
getNextDirection Left Horizontal = Left
getNextDirection Right Horizontal = Right

getDelta Down = (1, 0)
getDelta Up = (-1, 0)
getDelta Left = (0, -1)
getDelta Right = (0, 1)

getDirectionPieces Down = [UpLeft, UpRight, Vertical]
getDirectionPieces Up = [DownLeft, DownRight, Vertical]
getDirectionPieces Left = [UpRight, DownRight, Horizontal]
getDirectionPieces Right = [UpLeft, DownLeft, Horizontal]

add :: Position -> Position -> Position
add (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)

getNextPosition :: Position -> Direction -> Piece -> Position
getNextPosition position direction piece =
  let nextDirection = getNextDirection direction piece
      delta = getDelta nextDirection
      nextPosition = add position delta
   in nextPosition

nextPositionIs :: Position -> Direction -> Piece -> (Position -> Bool) -> Bool
nextPositionIs position direction piece pred =
  let nextPos = getNextPosition position direction piece
   in pred nextPos

findPaths :: (Member Shuffle r) => Int -> Int -> NonEmpty PathPiece -> Sem r [[PathPiece]]
findPaths rows cols xs@((PathPiece previousPiece previousPosition previousOutDirection) :| _) = do
  let currentPosition = add previousPosition (getDelta previousOutDirection)
  let directionPieces = getDirectionPieces previousOutDirection
  possiblePieces <- shuffle directionPieces
  let nextPositionIs' = nextPositionIs currentPosition previousOutDirection
  let isEdge (row, col) = row == -1 || col == -1 || row == rows || col == cols
  let isFree (row, col) = all (\(PathPiece _ pathPosition _) -> pathPosition /= (row, col) && row >= 0 && col >= 0 && row < rows && col < cols) (toList xs)
  let toEdgePieces = filter (\piece -> nextPositionIs' piece isEdge) possiblePieces
  let toFreePieces = filter (\piece -> nextPositionIs' piece isFree) possiblePieces
  nextPaths <- concat <$> mapM (\piece -> findPaths rows cols ((PathPiece piece currentPosition (getNextDirection previousOutDirection piece)) :| toList xs)) toFreePieces
  let completedPaths = map (\piece -> (PathPiece piece currentPosition (getNextDirection previousOutDirection piece)) : toList xs) toEdgePieces
  pure $ completedPaths ++ nextPaths

toChar :: Piece -> Char
toChar Horizontal = '═'
toChar Vertical = '║'
toChar UpLeft = '╝'
toChar UpRight = '╚'
toChar DownRight = '╔'
toChar DownLeft = '╗'

makeGridFromPath :: Int -> Int -> [PathPiece] -> Text
makeGridFromPath rows cols ps =
  let positionMap = M.fromList [(position, toChar piece) | (PathPiece piece position _) <- ps]
   in unlines [toText [M.findWithDefault '.' (row, col) positionMap | col <- [0 .. cols -1]] | row <- [0 .. rows -1]]

-- test2 =  makePaths 2 2

test :: IO ()
test = do
  let rows = 4
  let cols = 4
  paths <- runM . runShuffleIO $ makePaths rows cols
  forM_ paths $ \path ->
    putTextLn (makeGridFromPath rows cols path)

test2 :: IO ()
test2 = do
  let rows = 5
  let cols = 5
  paths <- runM . runShuffleIO $ makePaths rows cols
  let path = viaNonEmpty head (filter ((> 10) . length) paths)
  case path of
    Nothing -> putTextLn "couldnt find :("
    Just path -> putTextLn (makeGridFromPath rows cols path)