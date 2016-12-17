{-# LANGUAGE OverloadedStrings, RecordWildCards, GeneralizedNewtypeDeriving #-}

module PGMUD.Types.Adjective
    ( Adjective (..)
    , AdjectiveContext (..)
    , AdjectiveModifier (..)
    , AdjectiveId (..)
    , AdjectiveInteraction (..)
    ) where
    
import PGMUD.Prelude
import PGMUD.Types.Gear
import PGMUD.Types.Stats

import Data.Csv (FromNamedRecord(..), (.:), FromField(..), NamedRecord, Parser)

newtype AdjectiveId = AdjectiveId Text deriving (Eq, Ord, FromField)
data AdjectiveContext = ACWeapon | ACSkill
data AdjectiveModifier = AMElement Element | AMDerivedStat DerivedStat | AMBaseStat BaseStat
data AdjectiveInteraction = AIExclusive AdjectiveId | AILikesElement Element | AIDislikesElement Element | AIHatesElement Element | AINeedsElement Element | AILikesWeapon WeaponClass | AIDislikesWeapon WeaponClass
-- current proposal: excludes column, of semicolon-separated values. "interaction-ELEMENT" and "interaction-WEAPON", each with one of: blank, +, -, y, n (y/n element only: hates/needs)
    
instance FromField AdjectiveContext where
    parseField s
        | s == "weapon" = pure ACWeapon
        | s == "skill" = pure ACSkill
        | otherwise = mzero
    
data Adjective = Adjective
    { adjSortOrder :: Int
    , adjName :: Text
    , adjContext :: AdjectiveContext
    , adjModifiers :: [(Float, AdjectiveModifier)]
    , adjInteractions :: [AdjectiveInteraction]
    , adjId :: AdjectiveId
    , adjElem :: Maybe Element
    , adjWeapon :: Maybe WeaponClass
    , adjLevel :: ItemLevel
    }
    
newtype DefaultToZero = DefaultToZero Float deriving (Num, Eq, Ord)
instance FromField DefaultToZero where
    parseField "" = pure 0
    parseField s = parseField s
    
buildRange :: (Enum a, Bounded a, Nameable a) => NamedRecord -> Parser [(Float, a)]
buildRange m = mapM (\e -> ((\(DefaultToZero a) -> (a, e)) <$> (m .: name e))) [minBound..maxBound]

instance FromNamedRecord Adjective where
    parseNamedRecord m = let 
        so = m .: "sort order"
        aName = m .: "adjective"
        context = m .: "context"
        elements = buildRange m
        amElements = map (mapSnd AMElement) <$> elements
        amDerived = map (mapSnd AMDerivedStat) <$> buildRange m
        amBase = map (mapSnd AMBaseStat) <$> buildRange m
        modifiers = filter (\a -> fst a /= 0) <$> mconcat [amElements, amDerived, amBase]
        interactions = undefined
        adjid = m .: "id"
        element = (\a -> if fst a /= 0 then Just (snd a) else Nothing) <$> foldl' (\l r -> if fst l > fst r then l else r) (0, minBound) <$> elements
        weapon = m .: "weapon class"
        level = m .: "ilvl"
      in
        Adjective <$> so <*> aName <*> context <*> modifiers <*> interactions <*> adjid <*> element <*> weapon <*> level
    
instance Eq Adjective where
    (==) l r = adjId l == adjId r
    
instance Ord Adjective where
    compare l r = case compare (adjSortOrder l) (adjSortOrder r) of
        EQ -> compare (adjId l) (adjId r)
        LT -> LT
        GT -> GT
        
