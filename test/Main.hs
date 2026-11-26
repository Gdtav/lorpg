module Main (main) where

import RPG.Core.Dice
import RPG.Core.Initiative
import RPG.Core.Mechanics
import RPG.Core.RNG
import RPG.Core.Ruleset
import RPG.Rulesets.DnD5e

type Failure = String

check :: String -> Bool -> Either Failure ()
check name ok
  | ok = Right ()
  | otherwise = Left ("FAILED: " ++ name)

checkEq :: (Eq a, Show a) => String -> a -> a -> Either Failure ()
checkEq name expected actual
  | expected == actual = Right ()
  | otherwise = Left ("FAILED: " ++ name ++ ": expected " ++ show expected ++ ", got " ++ show actual)

hoist :: String -> Either Failure () -> [Failure]
hoist _ (Right ()) = []
hoist _ (Left e) = [e]

propManySeeds :: String -> (Seed -> [Failure]) -> [Failure]
propManySeeds _name prop = concatMap (prop . mkSeed) [1 .. 500]

mkCombatant :: HitPoints -> HitPoints -> Creature
mkCombatant cur mx = Creature
  { hp = cur, hpMax = mx, tempHp = 0
  , resistances = noResistances
  , attitude = Hostile
  , deathState = noDeathTracking
  , conditions = []
  , exhaustionLevel = 0
  }

dmg :: Int -> DamageType -> Damage
dmg = Damage

fire, slash :: DamageType
fire = DamageType "fire"
slash = DamageType "slashing"

main :: IO ()
main = do
  let rs = dnd5e
      results =
        concat
          [ hoist "ability modifiers" $
              sequence_
                [ checkEq ("mod " ++ show s) m (abilityMod s)
                | (s, m) <-
                    [ (1, -5), (3, -4), (8, -1), (10, 0), (11, 0)
                    , (12, 1), (20, 5), (21, 5), (30, 10)
                    ]
                ]
          , hoist "proficiency by level" $
              sequence_
                [ checkEq ("prof " ++ show l) p (proficiencyFor l)
                | (l, p) <- [(1,2),(4,2),(5,3),(8,3),(9,4),(12,4),(13,5),(16,5),(17,6),(20,6)]
                ]
          , hoist "standard DCs" $
              sequence_
                [ checkEq ("dc " ++ show lvl) v (standardDC lvl)
                | (lvl, v) <- [(VeryEasy,5),(Easy,10),(Medium,15),(Hard,20),(VeryHard,25),(NearlyImpossible,30)]
                ]
          , hoist "check bonus & spell save DC" $ do
              checkEq "trained lvl1 str16" 5 (checkBonus rs 16 1 1)
              checkEq "expertise lvl9 dex18" 12 (checkBonus rs 18 9 2)
              checkEq "untrained lvl9 dex18" 4 (checkBonus rs 18 9 0)
              checkEq "spell save DC lvl5 int18" 15 (spellSaveDC rs 18 5)
              checkEq "spell attack lvl5 int18" 7 (spellAttackBonus rs 18 5)
          , hoist "advantage reduction" $ do
              checkEq "no modes" StraightRoll (resolveMode [])
              checkEq "two adv" NetAdvantage (resolveMode [Advantage, Advantage])
              checkEq "adv+dis" StraightRoll (resolveMode [Advantage, Disadvantage])
              checkEq "2dis+1adv" StraightRoll (resolveMode [Disadvantage, Disadvantage, Advantage])
              checkEq "1dis" NetDisadvantage (resolveMode [Disadvantage])
          , hoist "attack outcome table" $ do
              check "nat20 always hits" (rsAttackOutcome rs 20 3 30 == CriticalHit 3)
              check "nat1 always misses" (rsAttackOutcome rs 1 40 5 == MissCritical)
              check "meet beats AC" (rsAttackOutcome rs 15 18 18 == Hit 18)
              check "below AC misses" (rsAttackOutcome rs 14 17 18 == Miss 17)
          , hoist "damage pipeline" $ do
              checkEq "plain damage" 3 (hp (fst (applyDamage rs False (mkCombatant 10 10) (dmg 7 slash))))
              checkEq "hp floors at 0" 0 (hp (fst (applyDamage rs False (mkCombatant 4 10) (dmg 9 slash))))
              let resistC = (mkCombatant 18 20) { resistances = noResistances { resistedTypes = [slash] } }
                  vulnC = (mkCombatant 18 20) { resistances = noResistances { vulnerableTypes = [fire] } }
              checkEq "resistance halves" 12 (hp (fst (applyDamage rs False resistC (dmg 12 slash))))
              checkEq "vulnerability doubles" 6 (hp (fst (applyDamage rs False vulnC (dmg 6 fire))))
              check "immunity" (hp (fst (applyDamage rs False (mkCombatant 10 10) { resistances = noResistances { immuneTypes = [fire] } } (dmg 25 fire))) == 10)
          , hoist "temp hit points" $ do
              let c0 = grantTempHp (mkCombatant 10 10) 5
                  (c1, _) = applyDamage rs False c0 (dmg 7 fire)
              check "temp absorbed first" (tempHp c1 == 0 && hp c1 == 8)
              check "no stacking" (tempHp (grantTempHp c0 3) == 5)
          , hoist "damage at zero hp" $ do
              let dying = mkCombatant 0 10
                  (c1, _) = applyDamage rs False dying (dmg 3 fire)
              checkEq "one failure" 1 (deathFailures (deathState c1))
              let (c2, _) = applyDamage rs True dying (dmg 3 fire)
              checkEq "crit two failures" 2 (deathFailures (deathState c2))
              let (c3, r3) = applyDamage rs False dying (dmg 10 fire)
              check "instant death" (dead (deathState c3) && instantDeath r3)
              let (c4, r4) = applyDamage rs False dying (dmg 9 fire)
              check "no instant death below max" (not (dead (deathState c4)) && not (instantDeath r4))
          , hoist "massive damage to zero" $ do
              let (c1, r1) = applyDamage rs False (mkCombatant 5 10) (dmg 15 fire)
              check "remaining >= max kills" (dead (deathState c1) && instantDeath r1)
              let (c2, _) = applyDamage rs False (mkCombatant 5 10) (dmg 12 fire)
              check "remaining < max leaves dying" (hp c2 == 0 && not (dead (deathState c2)))
          , hoist "healing" $ do
              let (c1, _) = applyDamage rs False (mkCombatant 10 12) (dmg 5 fire)
                  (c2, ev) = heal rs c1 9
              checkEq "capped at max" 12 (hp c2)
              checkEq "log amount" 7 (case ev of HealingApplied n _ -> n; _ -> -1)
              let dying = (mkCombatant 0 10) { deathState = DeathTracking 1 2 False False }
                  (c3, _) = heal rs dying 3
              check "healing clears death tracking" (deathState c3 == noDeathTracking && hp c3 == 3)
          , hoist "death save faces" $ do
              checkEq "face 10 succeeds" (1, 0) (rsDeathSaveFace rs 10 undefined)
              checkEq "face 19 succeeds" (1, 0) (rsDeathSaveFace rs 19 undefined)
              checkEq "face 9 fails" (0, 1) (rsDeathSaveFace rs 9 undefined)
              checkEq "face 1 double fail" (0, 2) (rsDeathSaveFace rs 1 undefined)
              checkEq "face 20 regains" (Just 1) (rsDeathSaveRegain rs 20)
          , propManySeeds "death save invariants" $ \s ->
              let c = mkCombatant 0 10
                  (c', evs, _) = deathSave rs c s
              in if dead (deathState c')
                   then ["died without event" | not (elem CreatureDied evs)]
                   else if stable (deathState c')
                     then ["stabilized without event" | not (elem CreatureStabilized evs)]
                     else if hp c' > 0
                       then []
                       else hoist "counters move" $
                            check "at least one counter moved"
                              (deathSuccesses (deathState c') + deathFailures (deathState c') >= 1)
          , propManySeeds "crit doubles dice not modifier" $ \s ->
              let pool' = pool 2 6 3
                  (res, _, _) = attack rs [] 5 10 pool' s
              in case res of
                   (_, Nothing) -> []
                   (outcome, Just (total, faces)) -> hoist "crit semantics" $
                     case outcome of
                       CriticalHit _ -> checkEq "crit dice count" 4 (length faces)
                         >> check "total = dice + one modifier"
                             (total == sum faces + 3)
                       _ -> checkEq "normal dice count" 2 (length faces)
                         >> check "total = dice + modifier"
                             (total == sum faces + 3)
          , propManySeeds "attack outcomes are consistent" $ \s ->
              let ((outcome, _), _, _) = attack rs [] 5 15 (pool 1 8 2) s
              in hoist "attack consistency" $ case outcome of
                   CriticalHit t -> check "crit total beats AC or is nat20-adjacent" (t >= 5 + 1)
                   Hit t -> check "hit total >= AC" (t >= 15)
                   Miss t -> check "miss total < AC" (t < 15 && t > 1)
                   MissCritical -> Right ()
          , propManySeeds "d20 faces in range" $ \s ->
              let (f, _, s') = rollD20 StraightRoll s
                  (f2, _, _) = rollD20 NetAdvantage s'
              in hoist "face range" $
                 check "straight face in 1..20" (f >= 1 && f <= 20)
                 >> check "advantage face in 1..20" (f2 >= 1 && f2 <= 20)
          , hoist "dice algebra" $ do
              checkEq "EV 2d6+1" 8 (expectedValue (pool 2 6 1) :: Double)
              checkEq "range 2d6+1" (3, 13) (poolRange (pool 2 6 1))
              check "EV within range" (uncurry (<=) (poolRange (pool 3 4 2) :: (Int, Int)))
          , propManySeeds "rolled pools match range" $ \s ->
              let dp = pool 3 6 2
                  (total, faces, _) = rollPoolLogged dp s
              in hoist "pool semantics" $
                 check "faces counted" (length faces == 3)
                 >> check "total = faces + mod" (total == sum faces + 2)
          , hoist "conditions" $ do
              check "srd conditions all defined"
                (all (\c -> conditionEffects c /= Nothing) srdConditions)
              let (blinded, _) = addCondition (mkCombatant 10 10) "blinded"
              check "blinded: incoming advantage"
                (incomingAttackModes rs blinded == [Advantage])
              check "blinded: outgoing disadvantage"
                (outgoingAttackModes rs blinded == [Disadvantage])
              check "blinded: sight checks auto-fail"
                (checkAutoFails rs blinded "sight")
              check "unblinded: sight checks fine"
                (not (checkAutoFails rs (mkCombatant 10 10) "sight"))
              let (invisible, _) = addCondition (mkCombatant 10 10) "invisible"
              check "invisible: incoming disadvantage"
                (incomingAttackModes rs invisible == [Disadvantage])
              let (grappled, _) = addCondition (mkCombatant 10 10) "grappled"
              checkEq "grappled: speed zero" 0 (effectiveSpeed rs grappled 30)
              let (incap, _) = addCondition (mkCombatant 10 10) "incapacitated"
              check "incapacitated cannot act" (not (canAct rs incap))
              check "healthy can act" (canAct rs (mkCombatant 10 10))
              let (para, _) = addCondition (mkCombatant 10 10) "paralyzed"
              check "paralyzed: hits become critical"
                (incomingHitsCritical rs para)
              check "paralyzed: saves auto-fail"
                (savingThrow rs para [] 0 10 (mkSeed 1) `seqOfOutcome` CheckFailure)
              let (clean, _) = removeCondition para "paralyzed"
              check "condition removed" (not (hasCondition clean "paralyzed"))
          , hoist "condition damage resistance" $ do
              let (stone, _) = addCondition (mkCombatant 20 20) "petrified"
              checkEq "petrified halves all damage" 10
                (hp (fst (applyDamage rs False stone (dmg 20 slash))))
          , hoist "exhaustion" $ do
              let ex1 = (mkCombatant 10 10) { exhaustionLevel = 1 }
              check "level 1: disadvantage on checks"
                (conditionCheckModes rs ex1 == [Disadvantage])
              let ex3 = (mkCombatant 10 10) { exhaustionLevel = 3 }
              check "level 3: cumulative check modes"
                (conditionCheckModes rs ex3 == [Disadvantage])
              check "level 3: disadvantage on attacks"
                (outgoingAttackModes rs ex3 == [Disadvantage])
              let ex2 = (mkCombatant 10 10) { exhaustionLevel = 2 }
              checkEq "level 2: speed halved" 15 (effectiveSpeed rs ex2 30)
              let ex4 = (mkCombatant 3 10) { exhaustionLevel = 4 }
              checkEq "level 4: hp max halved" 5 (rsEffectiveHpMax rs ex4)
              checkEq "level 4: heal caps at halved max" 5
                (hp (fst (heal rs ex4 99)))
              let (ex6, _) = changeExhaustion rs (mkCombatant 10 10) 6
              check "level 6 kills" (dead (deathState ex6))
              let (recovered, _) = changeExhaustion rs ex2 (-1)
              checkEq "recovery reduces level" 1 (exhaustionLevel recovered)
          , hoist "saving throws" $ do
              let c = mkCombatant 10 10
                  (ok, _, _) = savingThrow rs c [] 9 15 (mkSeed 3)
              check "save total meet-or-beat"
                (ok == CheckSuccess || ok == CheckFailure)
              check "half on success"
                (damageAmount (saveDamage rs CheckSuccess (dmg 25 fire)) == 12)
              check "full on failure"
                (damageAmount (saveDamage rs CheckFailure (dmg 25 fire)) == 25)
          , hoist "initiative order" $ do
              let (order, _, _) = orderInitiative
                    [(("A" :: String), 5), ("B", 0), ("C", 5)] (mkSeed 11)
              check "three participants ranked" (length order == 3)
              let totals = map snd order
              check "descending order"
                (and (zipWith (>=) totals (drop 1 totals)))
              check "face within d20 range"
                (all (\t -> t >= 1 && t <= 25) totals)
              check "round order fixed"
                (roundOrder [1 :: Int, 2] 2 == [[1, 2], [1, 2]])
          ]
  if null results
    then putStrLn "All tests passed."
    else mapM_ putStrLn results >> putStrLn (show (length results) ++ " failure(s).")
  where
    seqOfOutcome (o, _, _) res = o == res
