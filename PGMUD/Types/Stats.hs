{-# LANGUAGE OverloadedStrings #-}

module PGMUD.Types.Stats
    ( Element (..)
    , DerivedStat (..)
    , BaseStat (..)
    ) where
    
import PGMUD.Prelude
    
data DerivedStat = HP | PD | SD | AP | EV | ACC | PA | SA
    deriving (Eq, Ord, Enum, Bounded)
data BaseStat = Stamina | Willpower | Health | Defense | Offense | Mental | Physical | Mind | Body | Dexterity | Skill | Energy | Maneuver | Technique | Cleverness
    deriving (Eq, Ord, Enum, Bounded)
data Element = Air | Earth | Fire | Water | Order | Chaos | Light | Metal | Blood | Decay
    deriving (Eq, Ord, Enum, Bounded)
    
instance Nameable Element where
    name Air = "air"
    name Earth = "earth"
    name Fire = "fire"
    name Water = "water"
    name Order = "order"
    name Chaos = "chaos"
    name Light = "light"
    name Metal = "metal"
    name Blood = "blood"
    name Decay = "decay"
    
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