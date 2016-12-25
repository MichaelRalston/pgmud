{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies #-}
module PGMUD.Types.Elements
    ( Element (..)
    , ElementalAffinity (..)
    , HasElementalAffinities (..)
    ) where
    
import PGMUD.Prelude
import PGMUD.Types.GeneralClasses
    
data Element = Air | Earth | Fire | Water | Order | Chaos | Light | Metal | Blood | Decay
    deriving (Eq, Ord, Enum, Bounded, Show)
    
newtype ElementalAffinity = ElementalAffinity Float deriving (Num, Show, Fractional)
--data ElementalAffinities = ElementalAffinities [ElementalAffinity] deriving (Show)
    
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

class HasElementalAffinities a where
    elementalAffinities :: a -> (EIVWrapper ElementalAffinity)

instance EnumIndexedValues ElementalAffinity Element where
    data EIVWrapper ElementalAffinity = ElementalAffinities [ElementalAffinity] deriving (Show)
    wrapperConstructor = ElementalAffinities
    wrapperDestructor (ElementalAffinities ea) = ea
