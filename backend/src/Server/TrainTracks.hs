{-# LANGUAGE DeriveGeneric #-}

module Server.TrainTracks where

import Data.Aeson.Types

data TrainTracks = TrainTracks Text deriving (Eq, Show, Generic)

instance ToJSON TrainTracks

instance FromJSON TrainTracks
