{-# LANGUAGE DeriveGeneric #-}

module Server.Crossword where

import Data.Aeson.Types
import Data.Swagger

data Crossword = Crossword Int deriving (Eq, Show, Generic)

instance ToJSON Crossword

instance FromJSON Crossword

instance ToSchema Crossword
