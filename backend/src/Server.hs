{-# LANGUAGE TypeApplications #-}

module Server where

import qualified Data.Map.Strict as M
import Effects.KVS
import Effects.PuzzleCRUD
import Effects.Shuffle
import qualified Network.Wai.Handler.Warp as W
import Network.Wai.Middleware.Cors
import Polysemy
import Polysemy.Error
import Polysemy.State
import Polysemy.Trace
import qualified Prng
import Servant
import Servant.Server
import Server.Crossword
import Server.Puzzles
import Server.TrainTracks

initTrainTracks :: M.Map Int TrainTracks
-- initTrainTracks =
--   ( M.singleton
--       2
--       ( TrainTracks
--           { _gridSize = GridSize {_cols = 8, _rows = 8},
--             _startPos = GridPosition {_col = 0, _row = 3},
--             _endPos = GridPosition {_col = 7, _row = 3},
--             _rowCounts = [1, 2, 3, 4, 5],
--             _colCounts = [5, 4, 3, 2, 1]
--           }
--       )
--   )
initTrainTracks =
  ( M.singleton
      1
      ( TrainTracks
          { _gridSize = GridSize {_cols = 6, _rows = 6},
            _fixedCells =
              [ PlacedCell {_direction = DownLeft, _position = GridPosition {_col = 0, _row = 1}},
                PlacedCell {_direction = DownLeft, _position = GridPosition {_col = 3, _row = 0}},
                PlacedCell {_direction = DownRight, _position = GridPosition {_col = 2, _row = 5}}
              ],
            _rowCounts = [3, 3, 5, 3, 3, 4],
            _colCounts = [4, 5, 2, 4, 2, 4]
          }
      )
  )
    <> ( M.singleton
           2
           ( TrainTracks
               { _gridSize = GridSize {_cols = 6, _rows = 6},
                 _fixedCells =
                   [ PlacedCell {_direction = UpLeft, _position = GridPosition {_col = 0, _row = 3}},
                     PlacedCell {_direction = DownLeft, _position = GridPosition {_col = 2, _row = 5}},
                     PlacedCell {_direction = DownLeft, _position = GridPosition {_col = 4, _row = 4}}
                   ],
                 _rowCounts = [3, 4, 3, 2, 6, 5],
                 _colCounts = [5, 3, 4, 2, 5, 4]
               }
           )
       )
    <> ( M.singleton
           3
           ( TrainTracks
               { _gridSize = GridSize {_cols = 6, _rows = 6},
                 _fixedCells =
                   [ PlacedCell {_direction = DownLeft, _position = GridPosition {_col = 0, _row = 2}},
                     PlacedCell {_direction = Vertical, _position = GridPosition {_col = 3, _row = 2}},
                     PlacedCell {_direction = Vertical, _position = GridPosition {_col = 2, _row = 5}}
                   ],
                 _rowCounts = [4, 4, 6, 4, 1, 1],
                 _colCounts = [2, 2, 6, 4, 3, 3]
               }
           )
       )

initCrosswords :: M.Map Int Crossword
initCrosswords = (M.singleton 1 (Crossword 100))

type API = "puzzles" :> ("train-tracks" :> (PuzzleAPI TrainTracks :<|> RandomTrainTrack) :<|> "crosswords" :> (PuzzleAPI Crossword))

api :: Proxy API
api = Proxy

apiServer = (puzzleServer :<|> randomTrainTrackServer) :<|> puzzleServer

createApp :: IO Application
createApp = do
  kvsIORefTrainTracks <- newIORef initTrainTracks
  kvsIORefCrossword <- newIORef initCrosswords
  genIORef <- newIORef (Prng.mkState 111 222 333 444)
  return (serve api $ hoistServer api (\sem -> interpretServer sem kvsIORefTrainTracks kvsIORefCrossword genIORef) apiServer)
  where
    interpretServer sem kvsIORefTrainTracks kvsIORefCrossword genIORef =
      sem
        & withTrace
        & traceToIO
        & runServerWithIORef kvsIORefTrainTracks
        & withTrace
        & traceToIO
        & runShuffle3StateIORef genIORef
        & runServerWithIORef kvsIORefCrossword
        & runError @PuzzleError
        & runM
        & liftToHandler
    liftToHandler = Handler . ExceptT . (fmap handleErrors)
    handleErrors (Left x) = Left err404 {errBody = "Error"}
    handleErrors (Right value) = Right value

runServer :: IO ()
runServer = do
  app <- createApp
  W.run 8081 (simpleCors app)
