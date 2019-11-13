{-# LANGUAGE TypeApplications #-}

module Server where

import qualified Data.Map.Strict as M
import Effects.KVS
import qualified Network.Wai.Handler.Warp as W
import Polysemy
import Polysemy.Error
import Polysemy.State
import Servant
import Servant.Server
import Server.Crossword
import Server.Puzzles
import Server.TrainTracks

initTrainTracks :: M.Map Int TrainTracks
initTrainTracks = (M.singleton 2 (TrainTracks "hello"))

initCrosswords :: M.Map Int Crossword
initCrosswords = (M.singleton 1 (Crossword 100))

type API = "puzzles" :> ("train-tracks" :> (PuzzleAPI TrainTracks) :<|> "crosswords" :> (PuzzleAPI Crossword))

api :: Proxy API
api = Proxy

apiServer = puzzleServer :<|> puzzleServer

createApp :: IO Application
createApp = do
  kvsIORefTrainTracks <- newIORef initTrainTracks
  kvsIORefCrossword <- newIORef initCrosswords
  return (serve api $ hoistServer api (\sem -> interpretServer sem kvsIORefTrainTracks kvsIORefCrossword) apiServer)
  where
    interpretServer sem kvsIORefTrainTracks kvsIORefCrossword =
      sem
        & runKvsOnMapState
        & runStateIORef @(M.Map Int TrainTracks) kvsIORefTrainTracks
        & runKvsOnMapState
        & runStateIORef @(M.Map Int Crossword) kvsIORefCrossword
        & runError @Text
        & runM
        & liftToHandler
    liftToHandler = Handler . ExceptT . (fmap handleErrors)
    handleErrors (Left x) = Left err404 {errBody = "Error"}
    handleErrors (Right value) = Right value

main :: IO ()
main = do
  app <- createApp
  W.run 8081 app
