-- copied from https://blog.jez.io/profiling-in-haskell/

module Prng
  ( State,
    mkState,
    next,
    jump,
    longJump,
  )
where

import Data.Bits
import Data.Word
import Prelude hiding (State)

-- | The state of the generator.
--
-- The state must be seeded so that it is not everywhere zero. If you have
-- a 64-bit seed, we suggest to seed a splitmix64 generator and use its
-- output to fill s.
--
-- In Haskell, we've made this type opaque. See 'mkState' to construct a 'State'.
data State
  = State
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !Word64
  deriving (Show)

-- instance Show State where
--   showText (State s0 s1 s2 s3) =
--     "mkState " ++ (show s0) ++ " " ++ (show s1) ++ " " ++ (show s2) ++ " " ++ (show s3)

-- | Create an initial state from a seed.
--
-- Raises an exception if the initial seed is all zeros.
--
-- >>> mkState 0 0 0 0
-- *** Exception: The state must be seeded so that it is not zero everywhere.
-- >>> mkState 1 2 3 4
-- mkState 1 2 3 4
mkState :: Word64 -> Word64 -> Word64 -> Word64 -> State
mkState 0 0 0 0 =
  error "The state must be seeded so that it is not zero everywhere."
mkState s0 s1 s2 s3 = State s0 s1 s2 s3

-- | This is xoshiro256** 1.0, our all-purpose, rock-solid generator. It has
-- excellent (sub-ns) speed, a state (256 bits) that is large enough for
-- any parallel application, and it passes all tests we are aware of.
--
-- For generating just floating-point numbers, xoshiro256+ is even faster.
--
-- >>> let state = mkState 1 2 3 4
-- >>> next state
-- (11520,mkState 7 0 262146 211106232532992)
next :: State -> (Word64, State)
next (State s0 s1 s2 s3) =
  let result = ((s1 * 5) `rotateL` 7) * 9
      t = s1 `unsafeShiftL` 17
      s2' = s2 `xor` s0
      s3' = s3 `xor` s1
      s1' = s1 `xor` s2'
      s0' = s0 `xor` s3'
      s2'' = s2' `xor` t
      s3'' = s3' `rotateL` 45
   in (result, State s0' s1' s2'' s3'')

-- | This is the jump function for the generator. It is equivalent
-- to 2^128 calls to next(); it can be used to generate 2^128
-- non-overlapping subsequences for parallel computations.
--
-- Note: This function is not yet implemented in Haskell.
jump :: State -> State
jump (State _s0 _s1 _s2 _s3) = undefined

-- | This is the long-jump function for the generator. It is equivalent to
-- 2^192 calls to next(); it can be used to generate 2^64 starting points,
-- from each of which jump() will generate 2^64 non-overlapping
-- subsequences for parallel distributed computations.
--
-- Note: This function is not yet implemented in Haskell.
longJump :: State -> State
longJump (State _s0 _s1 _s2 _s3) = undefined
