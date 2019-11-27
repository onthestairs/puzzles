{-# LANGUAGE DeriveGeneric #-}

module Server.TrainTracks where

import Data.Aeson.Types
import Data.Swagger

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
