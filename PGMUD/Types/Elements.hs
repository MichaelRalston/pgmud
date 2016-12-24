{-# LANGUAGE OverloadedStrings #-}

module PGMUD.Types.Elements
    ( Element (..)
    , ElementalAffinity (..)
    , ElementalAffinities (..)
    ) where
    
import PGMUD.Prelude
    
data Element = Air | Earth | Fire | Water | Order | Chaos | Light | Metal | Blood | Decay
    deriving (Eq, Ord, Enum, Bounded, Show)
    
newtype ElementalAffinity = ElementalAffinity Float
data ElementalAffinities = ElementalAffinities [ElementalAffinity]
    
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
