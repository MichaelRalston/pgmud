{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module PGMUD.Types.Gear
    ( WeaponClass (..)
    , ItemLevel (..)
    ) where
    
import PGMUD.Prelude
import Data.Csv (FromField(..))
    
data WeaponClass = Staff | Book | Fist | Axe | Orb | Wand | Rapier | Spear | Bow | Dagger | Whip | TwoHanded | Thrown deriving (Eq, Ord, Bounded, Enum, Show)
newtype ItemLevel = ItemLevel Int deriving (Show, Num, Eq, Ord) -- obviously there needs to be data here, but ...

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
    parseField f = ItemLevel <$> parseField f
