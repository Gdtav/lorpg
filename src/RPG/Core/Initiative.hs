-- | Turn order for combat: an initiative roll per participant and a
-- stable ranking. The reference game treats initiative as a Dexterity
-- check; the core only sees a modifier per participant and orders the
-- totals from highest to lowest. Ties keep their relative input order
-- (the table decides among tied participants; a ruleset may instead
-- re-roll, which is just calling 'rollInitiative' again).
module RPG.Core.Initiative
  ( rollInitiative
  , orderInitiative
  , roundOrder
  ) where

import Data.List (sortBy)
import Data.Ord (comparing, Down (..))

import RPG.Core.Mechanics (rollD20)
import RPG.Core.RNG (Seed)
import RPG.Core.Ruleset (Modifier)
import RPG.Core.Types

-- | Roll one initiative total for one participant.
rollInitiative :: Modifier -> Seed -> (Int, LogEvent, Seed)
rollInitiative bonus seed0 =
  let (face, _events, s1) = rollD20 StraightRoll seed0
      total = face + bonus
  in (total, RolledDie 20 face total, s1)

-- | Roll initiative for a list of participants and rank them. Each
-- element is (identifier, roll modifier). Returns ids in turn order,
-- with their totals and per-participant logs. The seed is threaded in
-- list order, so the outcome is deterministic for a given seed.
orderInitiative
  :: [(a, Modifier)] -> Seed -> ([(a, Int)], [LogEvent], Seed)
orderInitiative participants seed0 =
  let go [] s acc evs0 = (reverse acc, evs0, s)
      go ((x, m) : rest) s acc evs0 =
        let (total, ev, s') = rollInitiative m s
        in go rest s' ((x, total) : acc) (evs0 ++ [ev])
      (rolled, evs, seed1) = go participants seed0 [] []
      ranked = sortBy (comparing (Down . snd)) rolled
  in (ranked, evs, seed1)

-- | Expand an initiative order into the sequence of turns for a whole
-- combat of n rounds, round-robin in the fixed order (the reference
-- rules keep the same order every round).
roundOrder :: [a] -> Int -> [[a]]
roundOrder order rounds = replicate rounds order
