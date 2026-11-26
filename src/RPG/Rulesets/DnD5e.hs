-- | The reference ruleset: mathematics of the System Reference Document
-- 5.1 for the 5th edition of the world's oldest tabletop fantasy game
-- (Wizards of the Coast, CC-BY-4.0). Only mechanics live here — no
-- content, names, spell lists, or lore.
--
-- Encoded formulas (all from the SRD core rules sections):
--
--   * ability modifier: @(score - 10) / 2@, rounding down
--   * proficiency bonus by level: @+2@ at levels 1-4, @+3@ at 5-8,
--     @+4@ at 9-12, @+5@ at 13-16, @+6@ at 17-20
--   * checks: @d20 + modifiers@ succeeds against a DC on meet-or-beat
--   * attacks: natural 20 always hits (critical), natural 1 always misses
--   * critical hits roll all damage dice twice, modifiers once
--   * damage: minimum 0; resistance halves, then vulnerability doubles;
--     neither stacks with itself
--   * death saves: DC 10, three successes stabilize, three failures kill,
--     natural 20 restores one hit point, natural 1 counts as two failures,
--     any damage at zero hit points causes a failure (two on a critical),
--     remaining damage greater than or equal to the hit-point maximum
--     causes instant death
module RPG.Rulesets.DnD5e
  ( dnd5e
  , abilityMod
  , proficiencyFor
  , checkBonus
  , spellSaveDC
  , spellAttackBonus
  , Difficulty(..)
  , standardDC
  ) where

import RPG.Core.Ruleset
import RPG.Core.Types

dnd5e :: Ruleset
dnd5e = Ruleset
  { rsName = "SRD 5.1"
  , rsCheckSucceeds = \total dc -> total >= dc
  , rsAttackOutcome = \natural total ac ->
      case natural of
        20 -> CriticalHit total
        1  -> MissCritical
        _ | total >= ac -> Hit total
          | otherwise  -> Miss total
  , rsContestOutcome = \a b ->
      case compare a b of
        GT -> ContestWin
        LT -> ContestLose
        EQ -> ContestTie
  , rsAbilityModifier = abilityMod
  , rsProficiencyBonus = proficiencyFor
  , rsExpertiseMultiplier = id
  , rsDamagePipeline =
      [ \d c -> if damageType d `elem` immuneTypes (resistances c)
                  then d { damageAmount = 0 } else d
      , \d c -> if damageType d `elem` resistedTypes (resistances c)
                  then d { damageAmount = damageAmount d `div` 2 } else d
      , \d c -> if damageType d `elem` vulnerableTypes (resistances c)
                  then d { damageAmount = damageAmount d * 2 } else d
      , \d _ -> d { damageAmount = max 0 (damageAmount d) }
      ]
  , rsDeathSavesEnabled = True
  , rsDeathSaveDC = 10
  , rsDeathThreshold = 3
  , rsDeathSaveFace = \face _ ->
      if face >= 10 then (1, 0) else if face == 1 then (0, 2) else (0, 1)
  , rsDeathSaveRegain = \face -> if face == 20 then Just 1 else Nothing
  , rsInstantDeathAt = \remaining hpMax' -> remaining >= hpMax'
  , rsZeroHpDamage = \criticalExtra -> 1 + criticalExtra
  , rsStableRegain = True
  }

-- | @(score - 10) / 2@ rounding down; valid for any integer score.
abilityMod :: Score -> Modifier
abilityMod score = (score - 10) `div` 2

-- | Proficiency bonus by character level (levels 1 through 20+).
proficiencyFor :: Level -> Int
proficiencyFor lvl = 2 + ((lvl - 1) `div` 4)

-- | Total modifier for a skill or saving-throw roll: ability modifier
-- plus proficiency times the expertise multiplier when trained.
checkBonus :: Ruleset -> Score -> Level -> Int -> Modifier
checkBonus rs score lvl mult
  | mult > 0 = abilityMod score + mult * rsProficiencyBonus rs lvl
  | otherwise = abilityMod score

-- | Save difficulty class for magical effects:
-- @8 + proficiency bonus + spellcasting ability modifier@.
spellSaveDC :: Ruleset -> Score -> Level -> DC
spellSaveDC rs score lvl = 8 + rsProficiencyBonus rs lvl + abilityMod score

spellAttackBonus :: Ruleset -> Score -> Level -> Modifier
spellAttackBonus rs score lvl = rsProficiencyBonus rs lvl + abilityMod score

-- | The canonical difficulty ladder of the reference game.
data Difficulty = VeryEasy | Easy | Medium | Hard | VeryHard | NearlyImpossible
  deriving (Eq, Show, Enum, Bounded)

standardDC :: Difficulty -> DC
standardDC d = 5 * (fromEnum d + 1)
