module Server.Puzzles where

import qualified Data.Map.Strict as M
import Data.Proxy
import Effects.KVS
import Polysemy
import Polysemy.Error
import Servant
import Server.TrainTracks

type PuzzleAPI a =
  Get '[JSON] (M.Map Int a)
    :<|> Capture "id" Int :> Get '[JSON] a
    :<|> ReqBody '[JSON] a :> Post '[JSON] a

puzzleServer ::
  (Members [KVS Int a, Error Text] r) =>
  ServerT (PuzzleAPI a) (Sem r)
puzzleServer =
  list
    :<|> fetch
    :<|> addAndFetch
  where
    addAndFetch todo = (add todo) >>= fetch

add ::
  (Member (KVS Int a) r) =>
  a ->
  Sem r Int
add todo = do
  let key = 2
  insertKvs key todo
  return key

list :: Member (KVS Int a) r => Sem r (M.Map Int a)
list = fmap M.fromList listAllKvs

fetch ::
  (Members [KVS Int a, Error Text] r) =>
  Int ->
  Sem r a
fetch id = getKvs id >>= \case
  Just todo -> pure todo
  Nothing -> throw "failed"
