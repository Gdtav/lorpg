{-# LANGUAGE BangPatterns #-}

-- | A small, pure, deterministic pseudo-random number generator.
--
-- The engine is deliberately RNG-agnostic: all game logic consumes
-- 'Seed' values and returns updated seeds, so every rule resolution is
-- a pure function. Callers can swap this generator for a cryptographically
-- secure one, an external dice roller, or a recorded log of physical die
-- rolls without touching any game logic.
module RPG.Core.RNG
  ( Seed(..)
  , mkSeed
  , nextWord
  , nextInt
  , rollWithin
  ) where

import Data.Bits (shiftR, xor)
import Data.Word (Word64)

-- | SplitMix64: chosen for tiny state, good statistical properties,
-- and jump-free purity (each step is a pure function of the previous state).
newtype Seed = Seed Word64
  deriving (Eq, Show)

mkSeed :: Word64 -> Seed
mkSeed = Seed

-- | Produce the next 64-bit value and the successor seed.
nextWord :: Seed -> (Word64, Seed)
nextWord (Seed s0) =
  let s1 = s0 + 0x9E3779B97F4A7C15
      z1 = mix s1
      z2 = mix (s1 + 1)
      z3 = mix (s1 + 2)
      s2 = s1 + 3
      result = z1 `xor` z2 `xor` z3
  in (result, Seed s2)
  where
    mix x =
      let a = (x `xor` (x `shiftR` 30)) * 0xBF58476D1CE4E5B9
          b = (a `xor` (a `shiftR` 27)) * 0x94D049BB133111EB
          c = b `xor` (b `shiftR` 31)
      in c

-- | Produce a uniformly distributed value in [0, n) from the full word.
nextInt :: Int -> Seed -> (Int, Seed)
nextInt n s =
  let (w, s') = nextWord s
      n' = fromIntegral (max 1 (toInteger n)) :: Integer
      v = fromIntegral w :: Integer
  in (fromIntegral (v `mod` n'), s')

-- | Roll an integer in [1, n] (a die with n sides).
rollWithin :: Int -> Seed -> (Int, Seed)
rollWithin n s =
  let (v, s') = nextInt n s
  in (v + 1, s')
