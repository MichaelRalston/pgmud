{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies, FlexibleInstances, UndecidableInstances, ScopedTypeVariables #-}
module PGMUD.Types.Elements
    ( Element (..)
    , HasElementalAffinities (..)
    ) where
    
import PGMUD.Prelude
import PGMUD.Types.GeneralClasses
    
data Element = Air | Earth | Fire | Water | Order | Chaos | Light | Metal | Blood | Decay
    deriving (Eq, Ord, Enum, Bounded, Show)
    
--newtype ElementalAffinity = ElementalAffinity Float deriving (Num, Show, Fractional)
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
    elementalAffinities :: a -> (EIVOuterWrapper Element)
    
instance HasEIV a Element => HasElementalAffinities a where
    elementalAffinities = getEIV
    
instance EnumIndexedValues Element where
    newtype EIVWrapper Element = ElementalAffinity Float deriving (Num, Show, Fractional)
    newtype EIVOuterWrapper Element = ElementalAffinities [EIVWrapper Element] deriving (Show)
    wrapperConstructor = ElementalAffinities
    wrapperDestructor (ElementalAffinities ea) = ea
    innerConstructor = ElementalAffinity
    innerDestructor (ElementalAffinity v) = v
