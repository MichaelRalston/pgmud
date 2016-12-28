{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module PGMUD.Types.Gear
    ( WeaponClass (..)
    , ItemLevel (..)
    , WeaponGroup (..)
    , isInGroup
    ) where
    
import PGMUD.Prelude
import Data.Csv (FromField(..))
    
data WeaponClass = Staff | Book | Fist | Axe | Orb | Wand | Rapier | Spear | Bow | Dagger | Whip | TwoHanded | Thrown deriving (Eq, Ord, Bounded, Enum, Show)
data WeaponGroup = Mystical | Large | Bladed | OneHanded | Ranged | Tricky deriving (Eq, Ord, Bounded, Enum, Show)
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

instance Nameable WeaponGroup where
    name Mystical = "mystical"
    name Large = "large"
    name Bladed = "bladed"
    name OneHanded = "onehanded"
    name Ranged = "ranged"
    name Tricky = "tricky"
    
isInGroup :: WeaponClass -> WeaponGroup -> Bool
isInGroup Staff Mystical = True
isInGroup Staff Large = True
isInGroup Book Mystical = True
isInGroup Fist Tricky = True
isInGroup Axe Bladed = True
isInGroup Orb Mystical = True
isInGroup Orb OneHanded = True
isInGroup Wand Mystical = True
isInGroup Wand OneHanded = True
isInGroup Rapier OneHanded = True
isInGroup Rapier Tricky = True
isInGroup Spear Large = True
isInGroup Bow Ranged = True
isInGroup Dagger OneHanded = True
isInGroup Dagger Bladed = True
isInGroup Whip Tricky = True
isInGroup TwoHanded Large = True
isInGroup TwoHanded Bladed = True
isInGroup Thrown Ranged = True
isInGroup _ _ = False
    
instance FromField ItemLevel where
    parseField f = ItemLevel <$> parseField f
