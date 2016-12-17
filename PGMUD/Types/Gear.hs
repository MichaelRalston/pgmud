{-# LANGUAGE OverloadedStrings #-}

module PGMUD.Types.Gear
    ( WeaponClass (..)
    , ItemLevel (..)
    ) where
    
import PGMUD.Prelude
import Data.Csv (FromField(..))
    
data WeaponClass = Staff | Book | Fist | Axe | Orb | Wand | Rapier | Spear | Bow | Dagger | Whip | TwoHanded | Thrown deriving (Eq, Ord, Bounded, Enum)
data ItemLevel = ItemLevel -- obviously there needs to be data here, but ...

instance Nameable WeaponClass where
    name Staff = "staff"
    name Book = "book"
    name Fist = "fist"
    name Axe = "axe"
    name Orb = "orb"
    name Wand = "wand"
    name Rapier = "rapier"
    name Spear = "spear"
    name Bow = "bow"
    name Dagger = "dagger"
    name Whip = "whip"
    name TwoHanded = "twohanded"
    name Thrown = "thrown"

instance FromField ItemLevel where
    parseField _ = pure $ ItemLevel

-- TODO: This is inefficient. it should probably just pattern match against the possible names? But that's a little repetitive for now, so atm it's defined in terms of Nameable.
instance FromField WeaponClass where
    parseField f = let c = foldl' (\l r -> if name l == f then l else r) minBound [minBound..maxBound]
      in
        if name c == f
            then pure c
            else mzero