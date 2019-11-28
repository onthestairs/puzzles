{-# LANGUAGE TemplateHaskell #-}

module Effects.Shuffle where

import Control.Monad
import qualified Data.Map.Strict as M
import Polysemy
import System.Random (randomRIO)

data Shuffle m a where
  Shuffle :: [a] -> Shuffle m [a]

makeSem ''Shuffle

-- shuffleList x =
--   if length x < 2
--     then return x
--     else do
--       i <- System.Random.randomRIO (0, length (x) -1)
--       r <- shuffle (take i x ++ drop (i + 1) x)
--       return (x !! i : r)

runShuffleIO ::
  (Member (Embed IO) r) =>
  Sem (Shuffle ': r) a ->
  Sem r a
runShuffleIO = interpret $ \case
  Shuffle xs -> pure $ xs

x = 3
-- test :: (Member Shuffle r) => Sem r (NonEmpty Int)
-- test = do
--   let xs = 1 :| [2 .. 5]
--   ys <- shuffle xs
--   pure ys
