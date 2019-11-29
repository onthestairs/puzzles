module Puzzles.TrainTracks where

import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Effects.Random
import Polysemy
import qualified Prng
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

data StartingEdge = TopEdge | BottomEdge | LeftEdge | RightEdge deriving (Show)

pickStartingPosition :: (Member Random r) => Int -> Int -> Sem r (StartingEdge, Position)
pickStartingPosition rows cols = do
  edge <- pick (TopEdge :| [BottomEdge, LeftEdge, RightEdge])
  randomCol <- pick (0 :| [1 .. cols - 1])
  randomRow <- pick (0 :| [1 .. rows - 1])
  pure $ case edge of
    TopEdge -> (TopEdge, (0, randomCol))
    BottomEdge -> (BottomEdge, (rows - 1, randomCol))
    LeftEdge -> (LeftEdge, (randomCol, 0))
    RightEdge -> (RightEdge, (randomCol, cols - 1))

topEdgePieces rows cols (row, col) = Vertical :| [] <> if row /= 0 then [UpLeft] else [] <> if row /= rows - 1 then [UpRight] else []

bottomEdgePieces rows cols (row, col) = Vertical :| [] <> if row /= 0 then [DownLeft] else [] <> if row /= rows - 1 then [DownRight] else []

leftEdgePieces rows cols (row, col) = Horizontal :| [] <> if col /= 0 then [UpLeft] else [] <> if col /= cols - 1 then [DownLeft] else []

rightEdgePieces rows cols (row, col) = Horizontal :| [] <> if row /= 0 then [UpRight] else [] <> if row /= cols - 1 then [DownRight] else []

findCandidatePieces rows cols (row, col) TopEdge = (topEdgePieces rows cols (row, col), Down)
findCandidatePieces rows cols (row, col) LeftEdge = (leftEdgePieces rows cols (row, col), Right)
findCandidatePieces rows cols (row, col) BottomEdge = (bottomEdgePieces rows cols (row, col), Up)
findCandidatePieces rows cols (row, col) RightEdge = (rightEdgePieces rows cols (row, col), Left)

pickStartingPieceAndDirection :: (Member Random r) => Int -> Int -> Position -> StartingEdge -> Sem r (Piece, Direction)
pickStartingPieceAndDirection rows cols (row, col) startingEdge = do
  let (candidatePieces, direction) = findCandidatePieces rows cols (row, col) startingEdge
  piece <- pick candidatePieces
  pure $ (piece, getNextDirection direction piece)

makePaths :: (Member Random r) => Int -> Int -> Sem r [[PathPiece]]
makePaths rows cols = do
  (startingEdge, startingPosition) <- pickStartingPosition rows cols
  (startingPiece, startingOutDirection) <- pickStartingPieceAndDirection rows cols startingPosition startingEdge
  let startingPathPiece = PathPiece {piece = startingPiece, position = startingPosition, outDirection = startingOutDirection}
  findPaths rows cols (startingPathPiece :| []) (Set.singleton startingPosition)

makeOnePath :: (Member Random r) => Int -> Int -> ([PathPiece] -> Bool) -> Sem r (Maybe [PathPiece])
makeOnePath rows cols pred = do
  paths <- makePaths rows cols
  let goodPaths = filter pred paths
  pure $ viaNonEmpty head goodPaths

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

getDirectionPieces Down = (UpLeft, UpRight, Vertical)
getDirectionPieces Up = (DownLeft, DownRight, Vertical)
getDirectionPieces Left = (UpRight, DownRight, Horizontal)
getDirectionPieces Right = (UpLeft, DownLeft, Horizontal)

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

tripleToList (x, y, z) = [x, y, z]

findPaths :: (Member Random r) => Int -> Int -> NonEmpty PathPiece -> Set.Set Position -> Sem r [[PathPiece]]
findPaths rows cols xs@((PathPiece previousPiece previousPosition previousOutDirection) :| _) visitedPositions = do
  let currentPosition = add previousPosition (getDelta previousOutDirection)
  let directionPieces = getDirectionPieces previousOutDirection
  possiblePieces <- tripleToList <$> shuffle3 directionPieces
  let nextPositionIs' = nextPositionIs currentPosition previousOutDirection
  let isEdge (row, col) = row == -1 || col == -1 || row == rows || col == cols
  let isFree (row, col) = (Set.notMember (row, col) visitedPositions) && row >= 0 && col >= 0 && row < rows && col < cols
  let toEdgePieces = filter (\piece -> nextPositionIs' piece isEdge) possiblePieces
  let toFreePieces = filter (\piece -> nextPositionIs' piece isFree) possiblePieces
  nextPaths <- concat <$> mapM (\piece -> findPaths rows cols ((PathPiece piece currentPosition (getNextDirection previousOutDirection piece)) :| toList xs) (Set.insert currentPosition visitedPositions)) toFreePieces
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

test2 :: IO ()
test2 = do
  let rows = 6
  let cols = 6
  let gen = (Prng.mkState 111 222 333 444)
  let paths = run . runRandomState gen $ makePaths rows cols
  let path = viaNonEmpty head (filter ((> 18) . length) paths)
  case path of
    Nothing -> putTextLn "couldnt find :("
    Just path -> putTextLn (makeGridFromPath rows cols path)

test3 :: IO ()
test3 = do
  let gen = Prng.mkState 111 222 333 444
  let zs = run . runRandomState gen $ shuffle3 (1, 2, 3)
  putTextLn ("hello")
  putTextLn (show zs)
