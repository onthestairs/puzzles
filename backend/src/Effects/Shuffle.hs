{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Effects.Shuffle where

import Control.Monad
import Polysemy
import Polysemy.State
import qualified Prng
import Prelude hiding (State, evalState, get, head, modify, put, runState, runStateIORef, tail)

data Shuffle3 m a where
  Shuffle3 :: (a, a, a) -> Shuffle3 m (a, a, a)

makeSem ''Shuffle3

runShuffleNoopIO ::
  (Member (Embed IO) r) =>
  Sem (Shuffle3 ': r) a ->
  Sem r a
runShuffleNoopIO = interpret $ \case
  Shuffle3 xs -> pure $ xs

getPerm :: (a, a, a) -> Int -> (a, a, a)
getPerm (x, y, z) 0 = (x, y, z)
getPerm (x, y, z) 1 = (x, z, y)
getPerm (x, y, z) 2 = (y, x, z)
getPerm (x, y, z) 3 = (y, z, x)
getPerm (x, y, z) 4 = (z, x, y)
getPerm (x, y, z) 5 = (z, y, x)

runShuffle3State :: Prng.State -> Sem (Shuffle3 ': r) a -> Sem r a
runShuffle3State s = fmap snd . runState s . reinterpret \case
  Shuffle3 xs -> do
    gen <- get
    let (word, gen') = Prng.next gen
    put gen'
    let i = fromIntegral word `mod` 6
    pure $ getPerm xs i

runShuffle3StateIORef :: (Member (Embed IO) r) => IORef Prng.State -> Sem (Shuffle3 ': r) a -> Sem r a
runShuffle3StateIORef s = runStateIORef s . reinterpret \case
  Shuffle3 xs -> do
    gen <- get
    let (word, gen') = Prng.next gen
    put gen'
    let i = fromIntegral word `mod` 6
    pure $ getPerm xs i
