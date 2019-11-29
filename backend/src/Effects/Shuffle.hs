{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Effects.Shuffle where

import Control.Monad
import Polysemy
import Polysemy.State
-- import qualified System.Random.Shuffle.FisherYates as FY

import qualified Prng
-- import System.Random
import Prelude hiding (State, evalState, get, head, modify, put, runState, tail)

data Shuffle3 m a where
  Shuffle3 :: (a, a, a) -> Shuffle3 m (a, a, a)

makeSem ''Shuffle3

runShuffleNoopIO ::
  (Member (Embed IO) r) =>
  Sem (Shuffle3 ': r) a ->
  Sem r a
runShuffleNoopIO = interpret $ \case
  Shuffle3 xs -> pure $ xs

-- shuffle3ToState ::
--   (Member (State Prng.State) r) =>
--   Sem (Shuffle3 ': r) a ->
--   Sem (r) a
-- -- runFisherYatesIO = interpret $ \case
-- --   Shuffle xs -> embed $ FY.shuffle xs
-- shuffle3ToState = interpret $ \case
--   -- Shuffle3 xs -> do
--   --   gen <- get @Prng.State
--   --   let (word, gen) = Prng.next gen
--   --   put gen
--   --   let i = fromIntegral word `mod` 6
--   --   pure $ getPerm xs i
--   Shuffle3 xs -> do
--     gen <- get
--     let (word, gen) = Prng.next (traceShowId gen)
--     -- let word = 304
--     put (traceShowId gen)
--     let i = fromIntegral word `mod` 6
--     pure $ getPerm xs i

getPerm :: (a, a, a) -> Int -> (a, a, a)
getPerm (x, y, z) 0 = (x, y, z)
getPerm (x, y, z) 1 = (x, z, y)
getPerm (x, y, z) 2 = (y, x, z)
getPerm (x, y, z) 3 = (y, z, x)
getPerm (x, y, z) 4 = (z, x, y)
getPerm (x, y, z) 5 = (z, y, x)

-- -- runShuffle3State
-- runShuffle3State :: Prng.State -> Sem (Shuffle3 ': r) a -> Sem r a
-- runShuffle3State s = evalState s . shuffle3ToState

runShuffle3State :: Prng.State -> Sem (Shuffle3 ': r) a -> Sem r a
runShuffle3State s = fmap snd . runState s . reinterpret \case
  Shuffle3 xs -> do
    gen <- get
    let (word, gen') = Prng.next gen
    put gen'
    let i = fromIntegral word `mod` 6
    pure $ getPerm xs i
-- runShuffle3State' :: Int -> Sem (Shuffle3 ': r) a -> Sem r a
-- runShuffle3State' s = fmap snd . runState s . reinterpret \case
--   Shuffle3 xs -> do
--     gen <- get
--     -- let (word, gen) =
--     -- let word = 304
--     put gen
--     let i = fromIntegral word `mod` 6
--     pure $ getPerm xs i
-- runIncrementingFilename :: Sem (FileNamer ': r) a -> Sem r a
-- runIncrementingFilename = fmap snd . runState 0 . reinterpret \case
-- fmap snd . runState 0 . reinterpret \case
--     GetName -> do
--         i <- get
--         modify ((+) 1)
--         pure $ show i
-- runFisherYatesIO ::
--   (Member (Embed IO) r) =>
--   Sem (Shuffle ': r) a ->
--   Sem r a
-- runFisherYatesIO = interpret $ \case
--   Shuffle xs -> embed $ FY.shuffle xs

-- shuffleBits :: Prng.State -> Board -> [a] -> ([a], Prng.State)
-- shuffleBits gen board 1 = (board, gen)
-- shuffleBits gen xs n =
--   let n' = n - 1
--       (rand, gen') = next gen
--       -- Uses `mod` instead of `randomR` to generate within a range
--       i = rand `mod` (fromIntegral n)
--       bs' = swapInList bs n' (fromIntegral i)
--    in shuffleBits gen' xs' n'

-- swapInList
--    :: [a]
--    -> Int
--    -> Int
--    -> [a]
--  swapBits bs i j | i == j = bs
--  swapBits bs i j =
--    let x = ((shiftR bs i) `xor` (shiftR bs j)) .&. 0x1
--    in  bs `xor` ((shiftL x i) .|. (shiftL x j))

-- x = 3
-- -- test :: (Member Shuffle r) => Sem r (NonEmpty Int)
-- -- test = do
-- --   let xs = 1 :| [2 .. 5]
-- --   ys <- shuffle xs
-- --   pure ys
