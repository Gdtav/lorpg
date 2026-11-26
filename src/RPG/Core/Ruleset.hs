-- | The ruleset abstraction: a record of functions and parameters that
-- defines one game system's mathematics. Everything in 'RPG.Core.Mechanics'
-- is written against this record, never against concrete rules, so any
-- d20-family game (or a differently shaped one) is a @Ruleset@ value away.
--
-- The reference implementation lives in "RPG.Rulesets.DnD5e" and encodes
-- the mechanics of the System Reference Document 5.1 (CC-BY-4.0).
module RPG.Core.Ruleset
  ( Score
  , Modifier
  , Level
  , DC
  , module RPG.Core.Types
  , Ruleset(..)
  ) where

import RPG.Core.Types

-- | An ability score value (1-30 in the SRD, but any Int here).
type Score = Int
-- | A bonus or penalty added to a die.
type Modifier = Int
-- | A character/class level, interpreted by the ruleset.
type Level = Int
-- | A difficulty class / target number.
type DC = Int

-- | The complete mathematics of one game system.
--
-- Field groupings:
--
-- * Scores & proficiency: 'rsAbilityModifier', 'rsProficiencyBonus',
--   'rsExpertiseFactor' (1 = normal, 2 = doubled, 0 = untrained adds nothing).
-- * Resolution: 'rsCheckSucceeds', 'rsAttackOutcome', 'rsContestOutcome'.
-- * Damage pipeline: ordered mitigation of one 'Damage' against a creature.
-- * Dying: whether the system tracks death saves and how each face resolves.
data Ruleset = Ruleset
  { rsName :: String
  , rsCheckSucceeds :: Int -> DC -> Bool
  , rsAttackOutcome :: Int -> Int -> DC -> AttackOutcome
  , rsContestOutcome :: Int -> Int -> ContestOutcome
  , rsAbilityModifier :: Score -> Modifier
  , rsProficiencyBonus :: Level -> Int
  , rsExpertiseMultiplier :: Int -> Int
  , rsDamagePipeline :: [Damage -> Creature -> Damage]
  , rsDeathSavesEnabled :: Bool
  , rsDeathSaveDC :: Int
  , rsDeathThreshold :: Int
  , rsDeathSaveFace :: Int -> Creature -> (Int, Int) -- ^ (successes, failures) delta
  , rsDeathSaveRegain :: Int -> Maybe HitPoints -- ^ faces that restore hp instead of counting
  , rsInstantDeathAt :: Int -> HitPoints -> Bool
  , rsZeroHpDamage :: Int -> Int -- ^ death-save failures caused by damage at 0 hp
  , rsStableRegain :: Bool -- ^ whether a stable dying creature eventually regains 1 hp
  , rsConditionEffects :: String -> Maybe ConditionEffects
    -- ^ how one named condition changes a creature's mechanics
  , rsExhaustionEffects :: Int -> ConditionEffects
    -- ^ cumulative-or-per-level effects of an exhaustion level
  , rsExhaustionKillLevel :: Int -- ^ the level at which exhaustion kills
  , rsEffectiveHpMax :: Creature -> HitPoints
    -- ^ hit-point maximum after system effects (e.g. halved at exhaustion 4)
  }
