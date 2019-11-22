module Swagger where

import Control.Lens
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Swagger
import Servant.Swagger
import Server

appSwagger :: Swagger
appSwagger =
  toSwagger api
    & info . title .~ "Games"
    & info . version .~ "1.0"
    & info . description ?~ "Puzzles"
    & info . license ?~ ("MIT" & url ?~ URL "http://mit.com")

writeSwaggerJSON :: IO ()
writeSwaggerJSON = writeFileLBS "../api/swagger.json" (encodePretty appSwagger)
