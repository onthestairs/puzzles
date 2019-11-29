{-# LANGUAGE DeriveGeneric #-}

module Server.TrainTracks where

import Data.Aeson.Types
import Data.Swagger
import qualified Puzzles.TrainTracks as TrainTracks

data Direction
  = Unknown
  | Horizontal
  | Vertical
  | DownLeft
  | DownRight
  | UpLeft
  | UpRight
  | Empty
  deriving (Eq, Show, Generic)

instance ToJSON Direction

instance FromJSON Direction

instance ToSchema Direction

data GridSize
  = GridSize
      { _cols :: Int,
        _rows :: Int
      }
  deriving (Eq, Show, Generic)

instance ToJSON GridSize

instance FromJSON GridSize

instance ToSchema GridSize

data GridPosition
  = GridPosition
      { _col :: Int,
        _row :: Int
      }
  deriving (Eq, Show, Generic)

instance ToJSON GridPosition

instance FromJSON GridPosition

instance ToSchema GridPosition

data PlacedCell
  = PlacedCell
      { _direction :: Direction,
        _position :: GridPosition
      }
  deriving (Eq, Show, Generic)

instance ToJSON PlacedCell

instance FromJSON PlacedCell

instance ToSchema PlacedCell

data TrainTracks
  = TrainTracks
      { _gridSize :: GridSize,
        _fixedCells :: [PlacedCell],
        _rowCounts :: [Int],
        _colCounts :: [Int]
      }
  deriving (Eq, Show, Generic)

instance ToJSON TrainTracks

instance FromJSON TrainTracks

instance ToSchema TrainTracks

pieceToDirection TrainTracks.UpLeft = UpLeft
pieceToDirection TrainTracks.UpRight = UpRight
pieceToDirection TrainTracks.DownLeft = DownLeft
pieceToDirection TrainTracks.DownRight = DownRight
pieceToDirection TrainTracks.Horizontal = Horizontal
pieceToDirection TrainTracks.Vertical = Vertical

pathPieceToPlacedCell (TrainTracks.PathPiece piece (row, col) _) = PlacedCell
  { _direction = pieceToDirection piece,
    _position = GridPosition
      { _row = row,
        _col = col
      }
  }

countOnRow :: [TrainTracks.PathPiece] -> Int -> Int
countOnRow pps row = length $ filter (\(TrainTracks.PathPiece _ (row', col') _) -> row' == row) pps

countOnCol :: [TrainTracks.PathPiece] -> Int -> Int
countOnCol pps col = length $ filter (\(TrainTracks.PathPiece _ (row', col') _) -> col' == col) pps

getRowCounts :: Int -> [TrainTracks.PathPiece] -> [Int]
getRowCounts rows pps = map (countOnRow pps) [0 .. rows -1]

getColCounts :: Int -> [TrainTracks.PathPiece] -> [Int]
getColCounts cols pps = map (countOnCol pps) [0 .. cols -1]

pathToTrainTracks :: Int -> Int -> [TrainTracks.PathPiece] -> TrainTracks
pathToTrainTracks rows cols path =
  let placedCells = map pathPieceToPlacedCell path
      fixedCells = catMaybes [viaNonEmpty head placedCells, viaNonEmpty last placedCells]
   in TrainTracks
        { _gridSize = GridSize
            { _rows = rows,
              _cols = cols
            },
          _fixedCells = fixedCells,
          _rowCounts = getRowCounts rows path,
          _colCounts = getColCounts rows path
        }
