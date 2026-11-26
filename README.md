# rpg-core

A lore-free, ruleset-agnostic **game-math engine** for d20-style tabletop
RPGs, written in Haskell. It implements the *mathematics* of tabletop
systems — dice, checks, contests, attacks, damage pipelines, healing,
and dying — while all system-specific formulas live in a single pluggable
`Ruleset` value. The bundled reference ruleset encodes the mechanics of
the **System Reference Document 5.1** (Wizards of the Coast, CC-BY-4.0);
no product identity, names, spell lists, or lore are included.

## Design

```
src/RPG/Core/RNG.hs        pure deterministic PRNG (SplitMix64), swappable
src/RPG/Core/Dice.hs       dice pools, logged rolls, EV/range algebra
src/RPG/Core/Types.hs      creature/HP/damage state, structured log events
src/RPG/Core/Ruleset.hs    the Ruleset record: one game system's formulas
src/RPG/Core/Mechanics.hs  resolution engine (checks, contests, attacks,
                           damage, healing, death saves) — ruleset-agnostic
src/RPG/Rulesets/DnD5e.hs  SRD 5.1 instantiation of Ruleset
```

Key invariants:

* **Pure everywhere.** All functions are total and deterministic;
  randomness is an explicit `Seed` threaded in and out, so game states
  are replayable and testable, and a UI can render any intermediate value.
* **Lore lives outside the engine.** The engine emits structured
  `LogEvent`s (rolls, outcomes, damage reports). Names, flavor text,
  and content packs are the caller's job.
* **Rules are data.** A different tabletop system is a new `Ruleset`
  value: supply its ability-score curve, proficiency table, hit/miss
  semantics, damage mitigation order, and death rules. Nothing in
  `Mechanics` references any concrete rule.

## Rules encoded by the SRD 5.1 ruleset

* Ability modifier: `(score − 10) / 2` rounding down.
* Proficiency bonus: +2 … +6 by level; added once per roll; expertise
  multiplies it (once), untrained adds nothing.
* Advantage/disadvantage: roll a second d20, take higher/lower; any
  combination of both cancels to a straight roll.
* Checks meet-or-beat the DC; contests compare two totals, ties keep
  the status quo.
* Attacks: natural 20 always hits as a critical (all damage dice rolled
  twice, modifiers once); natural 1 always misses.
* Damage: minimum 0; resistance halves, then vulnerability doubles;
  neither stacks; temp HP absorbs first; at 0 hp, damage causes a death
  save failure (two on a crit) and remaining damage ≥ hp max kills
  instantly.
* Death saves: DC 10; three successes stabilize, three failures die;
  natural 20 restores 1 hp, natural 1 counts as two failures; healing
  resets the counters.

## Building and testing

With GHC ≥ 9.2 and cabal-install:

```
cabal run rpg-core-test    # deterministic test suite
cabal run rpg-core-demo    # console demo of the math
```

Without cabal (plain GHC):

```
ghc -isrc -iapp -o demo app/Main.hs
ghc -isrc -itest -o test test/Main.hs
```

## Adding another system

Implement `Ruleset` from `RPG.Core.Ruleset` — everything else (advantage
reduction, damage ordering, death tracking, logs) is reused as-is:

```haskell
myRuleset = dnd5e
  { rsName = "My homebrew"
  , rsCheckSucceeds = \total dc -> total <= dc  -- roll-under, say
  , rsDeathSavesEnabled = False
  , ...
  }
```

## Attribution

Mechanics reference: *System Reference Document 5.1*, Wizards of the
Coast LLC, licensed under
[CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/). This library
contains no SRD content text.
