-- | Dice pools and die expressions, independent of any rule system.
--
-- A 'DicePool' is a count of identical dice plus a constant modifier
-- (e.g. @2d6 + 3@). Rule code decides how pools are rolled and applied;
-- this module only supplies the algebra.
module RPG.Core.Dice
  ( DicePool(..)
  , pool
  , d
  , rollPool
  , rollPoolLogged
  , expectedValue
  , poolRange
  ) where

import RPG.Core.RNG (Seed, rollWithin)

-- | @DicePool count sides modifier@ rolls @count@sides and adds @modifier@.
-- The modifier may be negative; results below zero are not clamped here
-- (clamping is a rule-system decision, see 'RPG.Core.Mechanics.applyDamage').
data DicePool = DicePool
  { diceCount   :: !Int
  , diceSides   :: !Int
  , poolMod     :: !Int
  }
  deriving (Eq, Show)

-- | Smart constructor validating sides and count.
pool :: Int -> Int -> Int -> DicePool
pool c s m
  | c < 0     = DicePool 0 s m
  | s < 1     = DicePool c 1 m
  | otherwise = DicePool c s m

-- | A single unmodified die: @d 6 == 1d6 + 0@.
d :: Int -> DicePool
d s = pool 1 s 0

-- | Roll a pool, returning the total.
rollPool :: DicePool -> Seed -> (Int, Seed)
rollPool dp s0 =
  let (total, _, s1) = rollPoolLogged dp s0 in (total, s1)

-- | Roll a pool, returning the total, every individual die face, and the
-- successor seed. Individual faces matter for rules such as critical-hit
-- damage doubling, which re-roll the dice but not the flat modifier.
rollPoolLogged :: DicePool -> Seed -> (Int, [Int], Seed)
rollPoolLogged (DicePool c sides m) = go c []
  where
    go 0 acc s = (sum acc + m, reverse acc, s)
    go n acc s =
      let (v, s') = rollWithin sides s
      in go (n - 1) (v : acc) s'

-- | Analytic expected value: @count * (sides+1)/2 + modifier@.
expectedValue :: DicePool -> Double
expectedValue (DicePool c s m) =
  fromIntegral c * (fromIntegral s + 1) / 2 + fromIntegral m

-- | @(minimum, maximum)@ total of a pool.
poolRange :: DicePool -> (Int, Int)
poolRange (DicePool c s m) =
  (c * 1 + m, c * s + m)
