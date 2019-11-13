{-# LANGUAGE DeriveGeneric #-}

module Server.Crossword where

import Data.Aeson.Types

data Crossword = Crossword Int deriving (Eq, Show, Generic)

instance ToJSON Crossword

instance FromJSON Crossword
