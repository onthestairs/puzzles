{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Effects.Random where

import Control.Monad
import Polysemy
import Polysemy.State
import qualified Prng
import Prelude hiding (State, evalState, get, modify, put, runState, runStateIORef)

data Random m a where
  Shuffle3 :: (b, b, b) -> Random m (b, b, b)
  Pick :: NonEmpty b -> Random m b

makeSem ''Random

runRandomNoopIO ::
  (Member (Embed IO) r) =>
  Sem (Random ': r) a ->
  Sem r a
runRandomNoopIO = interpret $ \case
  Shuffle3 xs -> pure $ xs
  Pick xs -> pure $ head xs

getPerm :: (a, a, a) -> Int -> (a, a, a)
getPerm (x, y, z) 0 = (x, y, z)
getPerm (x, y, z) 1 = (x, z, y)
getPerm (x, y, z) 2 = (y, x, z)
getPerm (x, y, z) 3 = (y, z, x)
getPerm (x, y, z) 4 = (z, x, y)
getPerm (x, y, z) 5 = (z, y, x)

randomToState :: Sem (Random ': r) a -> Sem (State Prng.State ': r) a
randomToState = reinterpret \case
  Shuffle3 xs -> do
    gen <- get
    let (word, gen') = Prng.next gen
    put gen'
    let i = fromIntegral word `mod` 6
    pure $ getPerm xs i
  Pick xs -> do
    let l = length xs
    gen <- get
    let (word, gen') = Prng.next gen
    put gen'
    let i = fromIntegral word `mod` l
    pure $ case viaNonEmpty head (drop i (toList xs)) of
      Nothing -> error "cant happen!"
      Just x -> x

runRandomState :: Prng.State -> Sem (Random ': r) a -> Sem r a
runRandomState s = fmap snd . runState s . randomToState

runRandomStateIORef :: (Member (Embed IO) r) => IORef Prng.State -> Sem (Random ': r) a -> Sem r a
runRandomStateIORef s = runStateIORef s . randomToState
