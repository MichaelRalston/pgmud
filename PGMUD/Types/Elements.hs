{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module PGMUD.Types.Elements
    ( Element (..)
    , ElementalAffinity (..)
    , ElementalAffinities (..)
    , HasElementalAffinities (..)
    ) where
    
import PGMUD.Prelude
    
data Element = Air | Earth | Fire | Water | Order | Chaos | Light | Metal | Blood | Decay
    deriving (Eq, Ord, Enum, Bounded, Show)
    
newtype ElementalAffinity = ElementalAffinity Float deriving (Num, Show)
data ElementalAffinities = ElementalAffinities [ElementalAffinity] deriving (Show)
    
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
    elementalAffinities :: a -> ElementalAffinities
    
instance Monoid ElementalAffinities where
    mempty = ElementalAffinities $ map (\_ -> ElementalAffinity 0) [minBound :: Element ..maxBound]
    mappend (ElementalAffinities l) (ElementalAffinities r) = ElementalAffinities $ zipWith (+) l r 