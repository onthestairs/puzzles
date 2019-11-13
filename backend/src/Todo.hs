{-# LANGUAGE DeriveGeneric #-}

module Todo where

import Data.Aeson.Types
import Data.Functor ((<&>))
import qualified Data.Map.Strict as M
import Effects.KVS
import GHC.Generics
-- import MonotonicSequence
import Polysemy
import Polysemy.Error

-- data TodoError = TodoNotAvailable Int

data TrainTracks = TrainTracks Text deriving (Eq, Show, Generic)

instance ToJSON TrainTracks

instance FromJSON TrainTracks
