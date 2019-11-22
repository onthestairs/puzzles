{-# LANGUAGE TypeApplications #-}

module Server where

import qualified Data.Map.Strict as M
import Effects.KVS
import Effects.PuzzleCRUD
import qualified Network.Wai.Handler.Warp as W
import Network.Wai.Middleware.Cors
import Polysemy
import Polysemy.Error
import Polysemy.State
import Polysemy.Trace
import Servant
import Servant.Server
import Server.Crossword
import Server.Puzzles
import Server.TrainTracks

initTrainTracks :: M.Map Int TrainTracks
initTrainTracks =
  ( M.singleton
      2
      ( TrainTracks
          { _gridSize = GridSize {_cols = 8, _rows = 8},
            _startPos = GridPosition {_col = 0, _row = 3},
            _endPos = GridPosition {_col = 7, _row = 3},
            _rowCounts = [1, 2, 3, 4, 5],
            _colCounts = [5, 4, 3, 2, 1]
          }
      )
  )

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
        & withTrace
        & traceToIO
        & runServerWithIORef kvsIORefTrainTracks
        & withTrace
        & traceToIO
        & runServerWithIORef kvsIORefCrossword
        & runError @PuzzleError
        & runM
        & liftToHandler
    liftToHandler = Handler . ExceptT . (fmap handleErrors)
    handleErrors (Left x) = Left err404 {errBody = "Error"}
    handleErrors (Right value) = Right value

main :: IO ()
main = do
  app <- createApp
  W.run 8081 (simpleCors app)
