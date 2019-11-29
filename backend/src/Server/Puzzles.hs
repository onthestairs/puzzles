{-# LANGUAGE TypeApplications #-}

module Server.Puzzles where

import qualified Data.Map.Strict as M
import Data.Proxy
import Effects.KVS
import Effects.PuzzleCRUD
import Effects.Random
import Polysemy
import Polysemy.Error
import Polysemy.State
import qualified Puzzles.TrainTracks as TrainTracks
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

type RandomTrainTrack = "random" :> QueryParam "rows" Int :> QueryParam "cols" Int :> Get '[JSON] TrainTracks

randomTrainTrackServer ::
  (Members '[Random, Error PuzzleError] r) =>
  ServerT (RandomTrainTrack) (Sem r)
randomTrainTrackServer maybeRows maybeCols = do
  let rows = fromMaybe 5 maybeRows
  let cols = fromMaybe 5 maybeCols
  maybePath <- TrainTracks.makeOnePath (traceShowId rows) (traceShowId cols) ((> 18) . length)
  case maybePath of
    Just path -> pure $ pathToTrainTracks rows cols path
    Nothing -> throw $ HTTP404 "couldnt find puzzle of that size"
