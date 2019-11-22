{-# LANGUAGE TemplateHaskell #-}

module Effects.PuzzleCRUD where

import Control.Monad
import qualified Data.Map.Strict as M
import Effects.KVS
import Polysemy
import Polysemy.Trace
import Prelude hiding (State, get, modify, trace)

data PuzzleCRUD k v m a where
  ListPuzzles :: PuzzleCRUD k v m (M.Map k v)
  FetchPuzzle :: k -> PuzzleCRUD k v m (Maybe v)
  AddPuzzle :: v -> PuzzleCRUD k v m v

makeSem ''PuzzleCRUD

runPuzzleCrudAsKVS ::
  (Member (KVS Int v) r) =>
  Sem ((PuzzleCRUD Int v) ': r) a ->
  Sem r a
runPuzzleCrudAsKVS = interpret $ \case
  ListPuzzles -> fmap M.fromList listAllKvs
  FetchPuzzle k -> getKvs k
  AddPuzzle v -> do
    let key = (2 :: Int)
    insertKvs key v
    return v

withTrace ::
  (Members [Trace, PuzzleCRUD Int v] r) =>
  Sem ((PuzzleCRUD Int v) ': r) a ->
  Sem r a
withTrace = interpret $ \case
  ListPuzzles -> do
    trace ("Listing puzzles")
    listPuzzles
  FetchPuzzle k -> do
    trace ("Fetching puzzle: " <> show k)
    fetchPuzzle k
  AddPuzzle v -> do
    trace ("Add puzzle")
    addPuzzle v
