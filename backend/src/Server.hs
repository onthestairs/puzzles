{-# LANGUAGE TypeApplications #-}

-- module Server where

-- import Control.Lens
-- import Data.Aeson
-- import Data.Aeson.Encode.Pretty (encodePretty)
-- -- import Data.IORef
-- import qualified Data.Map.Strict as M
-- import Data.Swagger
-- import Data.Text
-- import Data.Time.Calendar
-- import Effects.KVStore
-- import Network.Wai
-- import Network.Wai.Handler.Warp
-- import Network.Wai.Middleware.Cors
-- import Polysemy hiding (run)
-- import Polysemy.Error
-- import Servant
-- import Servant.Swagger
-- import Server.Puzzles

-- -- type UsersAPI = "users" :> Get '[JSON] [User]

-- -- data User
-- --   = User
-- --       { username :: String,
-- --         email :: String
-- --       }
-- --   deriving (Eq, Show, Generic)

-- -- instance ToJSON User

-- -- instance ToSchema User

-- -- users1 :: [User]
-- -- users1 =
-- --   [ User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1),
-- --     User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)
-- --   ]

-- -- server1 :: Server UserAPI1
-- -- server1 = pure users1

-- -- type API = "puzzles" :> "train-tracks" :> TrainTracksAPI
-- type API = TrainTracksAPI

-- api :: Proxy API
-- api = Proxy

-- initTrainTracks :: M.Map Int TrainTracks
-- initTrainTracks = (M.singleton 2 (TrainTracks "hello"))

-- -- createApp :: IO Application
-- -- createApp = do
-- --   kvsIORef <- newIORef initTrainTracks
-- --   pure $ serve api $ hoistServer api (\sem -> interpretServer sem kvsIORef) puzzleServer
-- --   where
-- --     interpretServer sem kvsIORef =
-- --       sem
-- --         & runKvsOnMapState
-- --         -- & evalState --  @(M.Map Int TrainTracks) (M.singleton (2, TrainTracks "hello"))
-- --         & runStateIORef @(M.Map Int TrainTracks) kvsIORef
-- --         & runError @Text
-- --         & runM
-- --         & liftToHandler
-- --     liftToHandler = Handler . ExceptT . (fmap handleErrors)
-- --     handleErrors (Left x) = Left err404 {errBody = encodeUtf8 x}
-- --     handleErrors (Right value) = Right value

-- createApp :: IO Application
-- createApp = do
--   kvsIORef <- newIORef initTrainTracks
--   return (serve api $ hoistServer api (\sem -> interpretServer sem kvsIORef) puzzleServer)
--   where
--     interpretServer sem kvsIORef =
--       sem
--         & runKvsOnMapState
--         & runStateIORef @(M.Map Int TrainTracks) kvsIORef
--         & runError @Text
--         & runM
--         & liftToHandler
--     liftToHandler = Handler . ExceptT . (fmap handleErrors)
--     handleErrors (Left x) = Left err404 {errBody = "error"}
--     handleErrors (Right value) = Right value

-- main :: IO ()
-- main = do
--   app <- createApp
--   run 8081 app
-- -- appSwagger :: Swagger
-- -- appSwagger =
-- --   toSwagger userAPI
-- --     & info . title .~ "Games"
-- --     & info . version .~ "1.0"
-- --     & info . description ?~ "This is an API that tests swagger integration"
-- --     & info . license ?~ ("MIT" & url ?~ URL "http://mit.com")

-- -- writeSwaggerJSON :: IO ()
-- -- writeSwaggerJSON = writeFileLBS "../api/swagger.json" (encodePretty appSwagger)

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

type API = "puzzles" :> ("train-tracks" :> (PuzzleAPI TrainTracks))

api :: Proxy API
api = Proxy

apiServer = puzzleServer

createApp :: IO Application
createApp = do
  kvsIORef <- newIORef initTrainTracks
  return (serve api $ hoistServer api (\sem -> interpretServer sem kvsIORef) apiServer)
  where
    interpretServer sem kvsIORef =
      sem
        & runKvsOnMapState
        & runStateIORef @(M.Map Int TrainTracks) kvsIORef
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
