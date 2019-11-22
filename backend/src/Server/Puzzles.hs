{-# LANGUAGE TypeApplications #-}

module Server.Puzzles where

import qualified Data.Map.Strict as M
import Data.Proxy
import Effects.KVS
import Effects.PuzzleCRUD
import Polysemy
import Polysemy.Error
import Polysemy.State
import Servant
import Server.TrainTracks

type PuzzleAPI a =
  Get '[JSON] (M.Map Int a)
    :<|> Capture "id" Int :> Get '[JSON] a
    :<|> ReqBody '[JSON] a :> Post '[JSON] a

data PuzzleError = HTTP404 Text

puzzleServer ::
  (Members '[PuzzleCRUD Int a, Error PuzzleError] r) =>
  ServerT (PuzzleAPI a) (Sem r)
puzzleServer =
  listPuzzles
    :<|> ( \x -> fetchPuzzle x >>= \case
             Just y -> pure y
             Nothing -> throw $ HTTP404 "couldnt find item"
         )
    :<|> addPuzzle

runServerWithIORef ref sem =
  sem & runPuzzleCrudAsKVS
    & runKvsOnMapState
    & runStateIORef ref
