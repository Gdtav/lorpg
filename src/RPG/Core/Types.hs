-- | Shared state and event types for any rule system built on the core:
-- creatures with hit points, damage events, and a structured log of what
-- the engine did. Everything here is mechanical; no names, lore, or flavor.
module RPG.Core.Types
  ( HitPoints
  , Creature(..)
  , Damage(..)
  , DamageType(..)
  , ResistanceProfile(..)
  , noResistances
  , Attitude(..)
  , DeathTracking(..)
  , noDeathTracking
  , CheckOutcome(..)
  , ContestOutcome(..)
  , AttackOutcome(..)
  , DamageReport(..)
  , LogEvent(..)
  ) where

-- | Current hit points are conventionally non-negative; the ruleset decides
-- how zero is handled (death saves, disabled state, and so on).
type HitPoints = Int

-- | A combatant's mechanical state. 'deathState' is only used by rulesets
-- that model dying (e.g. SRD 5.1 death saving throws); others ignore it.
data Creature = Creature
  { hp          :: !HitPoints
  , hpMax       :: !HitPoints
  , tempHp      :: !HitPoints
  , resistances :: ResistanceProfile
  , attitude    :: !Attitude
  , deathState  :: !DeathTracking
  }
  deriving (Eq, Show)

data Attitude = Hostile | Friendly | Neutral
  deriving (Eq, Show)

-- | Abstract damage channel. The core treats it as an opaque key;
-- the ruleset interprets it (e.g. fire vs. slashing for resistances).
data DamageType = DamageType String
  deriving (Eq, Ord, Show)

-- | One instance of damage before mitigation.
data Damage = Damage
  { damageAmount :: !Int
  , damageType   :: !DamageType
  }
  deriving (Eq, Show)

-- | Whether a creature resists or is vulnerable to a damage type.
-- Multiple sources never stack in the reference ruleset; the core keeps
-- this set-based and lets the ruleset decide the reduction math.
data ResistanceProfile = ResistanceProfile
  { resistedTypes    :: [DamageType]
  , vulnerableTypes  :: [DamageType]
  , immuneTypes      :: [DamageType]
  }
  deriving (Eq, Show)

noResistances :: ResistanceProfile
noResistances = ResistanceProfile [] [] []

data DeathTracking = DeathTracking
  { deathSuccesses :: !Int
  , deathFailures  :: !Int
  , stable         :: !Bool
  , dead           :: !Bool
  }
  deriving (Eq, Show)

noDeathTracking :: DeathTracking
noDeathTracking = DeathTracking 0 0 False False

data CheckOutcome = CheckSuccess | CheckFailure
  deriving (Eq, Show)

data ContestOutcome = ContestWin | ContestLose | ContestTie
  deriving (Eq, Show)

-- | Result of an attack roll against a target.
data AttackOutcome
  = MissCritical          -- ^ natural minimum (a 1 in the SRD)
  | Miss Int              -- ^ total below the target number
  | Hit Int               -- ^ meets-or-beats the target number
  | CriticalHit Int       -- ^ natural maximum, hits regardless
  deriving (Eq, Show)

-- | How much damage a single instance actually dealt after mitigation,
-- and how the creature's pool absorbed it.
data DamageReport = DamageReport
  { dealtToTemp     :: !Int
  , dealtToHp       :: !Int
  , hpRemaining     :: !Int
  , tempRemaining   :: !Int
  , instantDeath    :: !Bool
  }
  deriving (Eq, Show)

-- | Structured audit trail. A UI can render these with any flavor text
-- it likes; the engine only states what happened numerically.
data LogEvent
  = RolledDie Int Int Int          -- ^ sides, face, running total
  | RolledAdvantage Int Int Int    -- ^ faceA, faceB, chosen
  | RolledDisadvantage Int Int Int
  | CheckRolled Int Int CheckOutcome      -- ^ total, threshold, outcome
  | AttackRolled Int AttackOutcome        -- ^ natural die, outcome
  | DamageApplied Damage Damage DamageReport
  | HealingApplied Int HitPoints   -- ^ amount, hp after
  | DeathSaveRolled Int Bool       -- ^ face, success?
  | CreatureDied
  | CreatureStabilized
  deriving (Eq, Show)
