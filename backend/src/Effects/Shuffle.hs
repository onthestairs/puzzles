{-# LANGUAGE TemplateHaskell #-}

module Effects.Shuffle where

import Control.Monad
import Polysemy
import System.Random
import qualified System.Random.Shuffle.FisherYates as FY
import Prelude hiding (head, tail)

data Shuffle m a where
  Shuffle :: [a] -> Shuffle m [a]

makeSem ''Shuffle

runShuffleNoopIO ::
  (Member (Embed IO) r) =>
  Sem (Shuffle ': r) a ->
  Sem r a
runShuffleNoopIO = interpret $ \case
  Shuffle xs -> pure $ xs

runFisherYatesIO ::
  (Member (Embed IO) r) =>
  Sem (Shuffle ': r) a ->
  Sem r a
runFisherYatesIO = interpret $ \case
  Shuffle xs -> embed $ FY.shuffle xs

x = 3
-- test :: (Member Shuffle r) => Sem r (NonEmpty Int)
-- test = do
--   let xs = 1 :| [2 .. 5]
--   ys <- shuffle xs
--   pure ys
