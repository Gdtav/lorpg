-- | The game-math engine. Every function here is pure and ruleset-agnostic:
-- all system-specific numbers and formulas arrive through the 'Ruleset'
-- record, and all randomness arrives through a 'Seed' that is threaded
-- through explicitly. A desktop client calls these functions and renders
-- the returned 'LogEvent's with whatever presentation layer it wants.
module RPG.Core.Mechanics
  ( resolveMode
  , rollD20
  , abilityCheck
  , contest
  , attack
  , applyDamage
  , heal
  , grantTempHp
  , deathSave
  , stabilizeCheck
  ) where

import RPG.Core.Dice (DicePool (..), rollPoolLogged)
import RPG.Core.RNG (Seed, rollWithin)
import RPG.Core.Ruleset
import RPG.Core.Types

-- | Reduce a list of circumstances to one net roll mode. Implements the
-- reference rule that advantage and disadvantage cancel each other out
-- regardless of how many sources of each exist.
resolveMode :: [RollMode] -> NetRollMode
resolveMode modes =
  let adv = length (filter (== Advantage) modes)
      dis = length (filter (== Disadvantage) modes)
  in if adv > 0 && dis > 0
       then StraightRoll
       else if adv > 0 then NetAdvantage else if dis > 0 then NetDisadvantage
         else StraightRoll

-- | Roll a single d20 under a net mode, returning the natural face,
-- log events, and the successor seed.
rollD20 :: NetRollMode -> Seed -> (Int, [LogEvent], Seed)
rollD20 mode seed0 =
  let (a, s1) = rollWithin 20 seed0
  in case mode of
       StraightRoll ->
         (a, [RolledDie 20 a a], s1)
       NetAdvantage ->
         let (b, s2) = rollWithin 20 s1
             chosen = max a b
         in (chosen, [RolledAdvantage a b chosen, RolledDie 20 chosen chosen], s2)
       NetDisadvantage ->
         let (b, s2) = rollWithin 20 s1
             chosen = min a b
         in (chosen, [RolledDisadvantage a b chosen, RolledDie 20 chosen chosen], s2)

-- | A full check: d20 under the given circumstances plus a precomputed
-- modifier (ability modifier, proficiency, and any situational bonuses)
-- against a threshold. The ruleset decides meet-or-beat semantics.
abilityCheck
  :: Ruleset -> [RollMode] -> Modifier -> DC -> Seed
  -> (CheckOutcome, [LogEvent], Seed)
abilityCheck rs modes bonus dc seed0 =
  let (face, logs, s1) = rollD20 (resolveMode modes) seed0
      total = face + bonus
      outcome = if rsCheckSucceeds rs total dc then CheckSuccess else CheckFailure
  in (outcome, logs ++ [CheckRolled total dc outcome], s1)

-- | Two opposed checks; higher total wins, exact ties keep the status quo.
contest
  :: Ruleset
  -> ([RollMode], Modifier)
  -> ([RollMode], Modifier)
  -> Seed
  -> (ContestOutcome, ContestOutcome, [LogEvent], Seed)
contest rs (mA, modA) (mB, modB) seed0 =
  let (faceA, logsA, s1) = rollD20 (resolveMode mA) seed0
      (faceB, logsB, s2) = rollD20 (resolveMode mB) s1
      totalA = faceA + modA
      totalB = faceB + modB
      fromFirst = rsContestOutcome rs totalA totalB
      fromSecond = case fromFirst of
        ContestWin  -> ContestLose
        ContestLose -> ContestWin
        ContestTie  -> ContestTie
  in (fromFirst, fromSecond, logsA ++ logsB, s2)

-- | Resolve one attack: roll to hit, and on a hit roll damage.
-- On a critical hit the damage dice are rolled twice (the flat modifier
-- is added once), matching the reference critical rule. Damage types are
-- opaque to the core and interpreted by the ruleset's pipeline.
attack
  :: Ruleset -> [RollMode] -> Modifier -> DC -> DicePool -> Seed
  -> ((AttackOutcome, Maybe (Int, [Int])), [LogEvent], Seed)
attack rs modes atkBonus ac dmgPool seed0 =
  let (face, logs, s1) = rollD20 (resolveMode modes) seed0
      total = face + atkBonus
      outcome = rsAttackOutcome rs face total ac
  in case outcome of
       Miss _ -> ((outcome, Nothing), logs, s1)
       MissCritical -> ((outcome, Nothing), logs, s1)
       _ ->
         let dp = case outcome of
               CriticalHit _ -> dmgPool { diceCount = diceCount dmgPool * 2 }
               _ -> dmgPool
             (dmgTotal, faces, s2) = rollPoolLogged dp s1
         in ((outcome, Just (dmgTotal, faces)), logs ++ [AttackRolled face outcome], s2)

-- | Apply one instance of damage to a creature.
--
-- Order of operations (matching the reference rules):
--
--   1. The ruleset pipeline mitigates the raw damage (immunity,
--      resistance, vulnerability, flat reductions), clamped to a
--      minimum of zero.
--   2. Temporary hit points absorb damage first; the remainder carries
--      over to current hit points, which floor at zero.
--   3. If the creature is at zero hit points when damaged, the ruleset
--      prescribes death-save failures (doubled for a critical hit).
--   4. If remaining damage at zero hit points equals or exceeds the
--      hit-point maximum, the creature dies instantly.
applyDamage
  :: Ruleset -> Bool -> Creature -> Damage -> (Creature, DamageReport)
applyDamage rs critical c0 dmg0 =
  let mitigated = foldr (\step d -> step d c0) dmg0 (rsDamagePipeline rs)
      raw = max 0 (damageAmount mitigated)
      absorbedByTemp = min (tempHp c0) raw
      toHp = raw - absorbedByTemp
      c1 = c0 { tempHp = tempHp c0 - absorbedByTemp }
      hp' = max 0 (hp c0 - toHp)
      remaining = toHp - (hp c0 `max` 0)
      wasDown = hp c0 <= 0
      instant = rsInstantDeathAt rs remaining (hpMax c0)
      c2 = c1 { hp = hp' }
      c3 = if wasDown && raw > 0
             then let f = rsZeroHpDamage rs (if critical then 1 else 0)
                      ds0 = deathState c2
                      ds1 = ds0 { deathFailures = deathFailures ds0 + f
                                , stable = False }
                  in c2 { deathState = ds1 }
             else c2
      c4 = if instant
             then c3 { deathState = (deathState c3) { dead = True } }
             else c3
      report = DamageReport
        { dealtToTemp = absorbedByTemp
        , dealtToHp = min toHp (hp c0 `max` 0)
        , hpRemaining = hp c4
        , tempRemaining = tempHp c4
        , instantDeath = instant
        }
  in (c4, report)

-- | Restore hit points, capped at the maximum. Receiving healing clears
-- death-save tracking in the reference rules.
heal :: Creature -> HitPoints -> (Creature, LogEvent)
heal c amount =
  let gained = min amount (hpMax c - hp c)
      c' = c { hp = hp c + gained
             , deathState = noDeathTracking
             }
  in (c', HealingApplied gained (hp c'))

-- | Grant temporary hit points. They never stack: a creature keeps
-- either its current buffer or the new one, whichever is larger.
grantTempHp :: Creature -> HitPoints -> Creature
grantTempHp c amount = c { tempHp = max (tempHp c) amount }

-- | Make one death saving throw for a dying creature at zero hit points.
-- The ruleset interprets the face (e.g. a 20 restores one hit point and
-- a 1 counts as two failures). Returns the updated creature and events.
deathSave :: Ruleset -> Creature -> Seed -> (Creature, [LogEvent], Seed)
deathSave rs c seed0
  | not (rsDeathSavesEnabled rs) = (c, [], seed0)
  | hp c > 0 || dead ds || stable ds = (c, [], seed0)
  | otherwise =
      let (face, s1) = rollWithin 20 seed0
          (dsucc, dfail) = rsDeathSaveFace rs face c
          ds' = ds { deathSuccesses = deathSuccesses ds + dsucc
                   , deathFailures = deathFailures ds + dfail }
          threshold = rsDeathThreshold rs
      in case rsDeathSaveRegain rs face of
         Just n ->
           let (c', ev) = heal c n
           in (c', [DeathSaveRolled face True, ev], s1)
         Nothing
           | deathSuccesses ds' >= threshold ->
               (c { deathState = ds' { stable = True, deathSuccesses = 0, deathFailures = 0 } }
               , [DeathSaveRolled face True, CreatureStabilized], s1)
           | deathFailures ds' >= threshold ->
               (c { deathState = ds' { dead = True } }
               , [DeathSaveRolled face (dfail == 0), CreatureDied], s1)
           | otherwise ->
               (c { deathState = ds' }
               , [DeathSaveRolled face (dfail == 0)], s1)
  where ds = deathState c

-- | A stabilization attempt administered by an ally: an ordinary check
-- against the ruleset's death-save DC. On success the creature is stable.
stabilizeCheck
  :: Ruleset -> [RollMode] -> Modifier -> Creature -> Seed
  -> (CheckOutcome, Creature, [LogEvent], Seed)
stabilizeCheck rs modes bonus target seed0 =
  let (outcome, logs, s1) = abilityCheck rs modes bonus (rsDeathSaveDC rs) seed0
      target' = case outcome of
        CheckSuccess | rsDeathSavesEnabled rs ->
          target { deathState = (deathState target) { stable = True } }
        _ -> target
      extra = [CreatureStabilized | outcome == CheckSuccess && rsDeathSavesEnabled rs]
  in (outcome, target', logs ++ extra, s1)
