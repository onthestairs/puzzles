{-# LANGUAGE DeriveGeneric #-}

module Server.TrainTracks where

import Data.Aeson.Types
import Data.Swagger

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

data TrainTracks
  = TrainTracks
      { _gridSize :: GridSize,
        _startPos :: GridPosition,
        _endPos :: GridPosition,
        _rowCounts :: [Int],
        _colCounts :: [Int]
      }
  deriving (Eq, Show, Generic)

instance ToJSON TrainTracks

instance FromJSON TrainTracks

instance ToSchema TrainTracks
