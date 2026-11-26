-- | A tiny console demo: two combatants exchange attacks and the engine's
-- structured log is rendered as plain text. In a real desktop application
-- this rendering layer would be a GUI and the lore/ruleset layer would
-- come from content packs; the engine underneath stays identical.
module Main (main) where

import RPG.Core.Dice
import RPG.Core.Mechanics
import RPG.Core.RNG
import RPG.Core.Ruleset
import RPG.Core.Types
import RPG.Rulesets.DnD5e

render :: LogEvent -> String
render ev = case ev of
  RolledDie _ face total -> "  rolled " ++ show face ++ " (total " ++ show total ++ ")"
  RolledAdvantage a b c -> "  advantage " ++ show a ++ "/" ++ show b ++ " -> " ++ show c
  RolledDisadvantage a b c -> "  disadvantage " ++ show a ++ "/" ++ show b ++ " -> " ++ show c
  CheckRolled total dc o ->
    "  check total " ++ show total ++ " vs DC " ++ show dc ++ ": " ++ show o
  AttackRolled face o ->
    "  attack die " ++ show face ++ ": " ++ show o
  DamageApplied raw final rep ->
    "  damage " ++ show (damageAmount raw) ++ " -> " ++ show (damageAmount final)
      ++ " (hp " ++ show (hpRemaining rep) ++ ", temp " ++ show (tempRemaining rep) ++ ")"
  HealingApplied n hp' -> "  healed " ++ show n ++ " (hp " ++ show hp' ++ ")"
  DeathSaveRolled face ok ->
    "  death save " ++ show face ++ (if ok then " (success)" else " (failure)")
  CreatureDied -> "  the creature dies"
  CreatureStabilized -> "  the creature stabilizes"

strike :: String -> [RollMode] -> Int -> DicePool -> Creature -> Seed
     -> IO (Creature, Seed)
strike label modes atkBonus pool' target seed0 = do
  putStrLn (label ++ ":")
  let ((outcome, mdmg), logs, seed1) =
        attack dnd5e modes atkBonus (ac target) pool' seed0
  mapM_ (putStrLn . render) logs
  (target', seed2) <- case (outcome, mdmg) of
    (CriticalHit _, Just (t, faces)) -> do
      putStrLn ("  CRITICAL! damage faces " ++ show faces ++ " -> " ++ show t)
      applyTo target (Damage t (DamageType "slashing")) seed1
    (Hit _, Just (t, faces)) -> do
      putStrLn ("  hit: damage faces " ++ show faces ++ " -> " ++ show t)
      applyTo target (Damage t (DamageType "slashing")) seed1
    _ -> putStrLn "  no damage" >> pure (target, seed1)
  pure (target', seed2)
  where
    applyTo tgt dm s = do
      let (tgt', rep) = applyDamage dnd5e False tgt dm
      putStrLn ("  target hp " ++ show (hpRemaining rep)
                ++ (if instantDeath rep then " (instant death!)" else ""))
      pure (tgt', s)
    ac _ = 15

main :: IO ()
main = do
  putStrLn "== rpg-core demo (SRD 5.1 mathematics) =="
  let str16 = 3
      lvl5Prof = proficiencyFor 5
      longsword = pool 1 8 str16
      firebolt = pool 1 10 0
  putStrLn ("attacker: +5 to hit (Str 16, prof " ++ show lvl5Prof ++ ")")
  putStrLn ("target: AC 15, 22 hp\n")
  let target0 = Creature { hp = 22, hpMax = 22, tempHp = 0
                         , resistances = noResistances
                         , attitude = Hostile
                         , deathState = noDeathTracking }
  (target1, seed1) <- strike "sneaky strike (advantage)" [Advantage] (str16 + lvl5Prof)
                         longsword target0 (mkSeed 42)
  putStrLn ""
  (_target2, _seed2) <- strike "reckless shot (disadvantage)" [Disadvantage, Advantage]
                         (str16 + lvl5Prof) firebolt target1 seed1
  putStrLn ""
  putStrLn "ability check vs DC 15, mod +7, both advantage and disadvantage sources:"
  let (outcome, logs, _) = abilityCheck dnd5e [Advantage, Disadvantage] 7 15 (mkSeed 7)
  mapM_ (putStrLn . render) logs
  putStrLn ("  outcome: " ++ show outcome)
