{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, TypeFamilies, GeneralizedNewtypeDeriving, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module PGMUD.Types.Stats
    ( DerivedStat (..)
    , BaseStat (..)
    , HasStatModifiers (..)
    ) where
    
import PGMUD.Prelude
import PGMUD.Types.GeneralClasses
    
data DerivedStat = HP | PD | SD | AP | EV | ACC | PA | SA
    deriving (Eq, Ord, Enum, Bounded, Show)
data BaseStat = Stamina | Willpower | Health | Defense | Offense | Mental | Physical | Mind | Body | Dexterity | Skill | Energy | Maneuver | Technique | Cleverness
    deriving (Eq, Ord, Enum, Bounded, Show)

instance Nameable DerivedStat where
    name HP = "hp"
    name PD = "pd"
    name SD = "sd"
    name AP = "ap"
    name EV = "ev"
    name ACC = "acc"
    name PA = "pa"
    name SA = "sa"
    
instance Nameable BaseStat where
    name Stamina = "stamina"
    name Willpower = "willpower"
    name Health = "health"
    name Defense = "defense"
    name Offense = "offense"
    name Mental = "mental"
    name Physical = "physical"
    name Mind = "mind"
    name Body = "body"
    name Dexterity = "dexterity"
    name Skill = "skill"
    name Energy = "energy"
    name Maneuver = "maneuver"
    name Technique = "technique"
    name Cleverness = "cleverness"
    
instance EnumIndexedValues BaseStat where
    newtype EIVWrapper BaseStat = BaseStatModifier Float deriving (Num, Show, Fractional)
    newtype EIVOuterWrapper BaseStat = BaseStatModifiers [EIVWrapper BaseStat] deriving (Show)
    wrapperConstructor = BaseStatModifiers
    wrapperDestructor (BaseStatModifiers ea) = ea
    innerConstructor = BaseStatModifier
    innerDestructor (BaseStatModifier v) = v

instance EnumIndexedValues DerivedStat where
    newtype EIVWrapper DerivedStat = DerivedStatModifier Float deriving (Num, Show, Fractional)
    newtype EIVOuterWrapper DerivedStat = DerivedStatModifiers [EIVWrapper DerivedStat] deriving (Show)
    wrapperConstructor = DerivedStatModifiers
    wrapperDestructor (DerivedStatModifiers ea) = ea
    innerConstructor = DerivedStatModifier
    innerDestructor (DerivedStatModifier v) = v

class HasStatModifiers a where
    baseStatModifiers :: a -> (EIVOuterWrapper BaseStat)
    derivedStatModifiers :: a -> (EIVOuterWrapper DerivedStat)
    
instance (HasEIV a BaseStat, HasEIV a DerivedStat) => HasStatModifiers a where
    baseStatModifiers = getEIV
    derivedStatModifiers = getEIV
