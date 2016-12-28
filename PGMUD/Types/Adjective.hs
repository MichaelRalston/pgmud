{-# LANGUAGE OverloadedStrings, RecordWildCards, GeneralizedNewtypeDeriving, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, ConstraintKinds, FlexibleContexts, ScopedTypeVariables, FunctionalDependencies #-}

module PGMUD.Types.Adjective
    ( Adjective (..)
    , AdjectiveContext (..)
    , AdjectiveModifier (..)
    , AdjectiveId (..)
    , AdjectiveInteraction (..)
    , AdjectiveType (..)
    , CanHasEIV
    , SkillClassification (..)
    , AdjectiveList (..)
    , ElementOrWeapon (..)
    , unwrapAdjectiveList    
    ) where
    
import PGMUD.Prelude
import PGMUD.Types.GeneralClasses
import PGMUD.Types.Gear
import PGMUD.Types.Stats
import PGMUD.Types.Elements
import PGMUD.Types.Effect

import Data.Csv (FromNamedRecord(..), (.:), FromField(..), NamedRecord, Parser)
import qualified Data.ByteString.Char8 as BSC

newtype SkillEfficiency = SkillEfficiency () deriving (Show, Eq, Ord, FromField, Bounded, Enum)

instance Nameable SkillEfficiency where
    name (SkillEfficiency ()) = "efficiency"

data SkillClassification = SCPiercing | SCHoming | SCHeal | SCStrike | SCCharge | SCBlock | SCParry | SCDodge | SCFlurry | SCSmash | SCBerserk deriving (Enum, Ord, Show, Eq, Bounded)

instance Nameable SkillClassification where
    name SCPiercing = "piercing"
    name SCHoming = "homing"
    name SCHeal = "heal"
    name SCStrike = "strike"
    name SCCharge = "charge"
    name SCBlock = "block"
    name SCParry = "parry"
    name SCDodge = "dodge"
    name SCFlurry = "flurry"
    name SCSmash = "smash"
    name SCBerserk = "berserk"

newtype AdjectiveId = AdjectiveId Text deriving (Eq, Ord, FromField, Show)
data AdjectiveContext = ACWeapon | ACSkill deriving (Show)
newtype Magnitude = Magnitude () deriving (Eq, Ord, Bounded, Enum, FromField, Show)

instance Nameable Magnitude where
    name (Magnitude ()) = "magnitude"
    
data AdjectiveModifier = AMElement Element | AMDerivedStat DerivedStat | AMBaseStat BaseStat | AMEfficiency SkillEfficiency | AMMagnitude Magnitude deriving (Show)
data AdjectiveType = ATWeaponClass | ATWeaponElement | ATSkillWeaponOrDamage | ATInvisibleDamageType | ATSkillMagnitude | ATChargeEffect | ATParryEffect | ATWeaponQuality | ATSkillQuality | ATSkillClassification | ATSkillElement deriving (Show, Eq, Ord)
data ElementOrWeapon = EOWElement Element | EOWWeapon WeaponClass deriving (Eq, Show, Ord)
data AdjectiveInteraction = AINoInteraction | AIExclusive AdjectiveId | AILikes ElementOrWeapon | AIDislikes ElementOrWeapon | AIHates ElementOrWeapon | AINeeds ElementOrWeapon deriving (Eq, Show)
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
    , adjClassification :: Maybe SkillClassification
    , adjDamageType :: Maybe DamageType
    , adjEffect :: Maybe Effect
    } deriving (Show)
    
{-newtype DefaultToZero a = DefaultToZero a deriving (Num, Eq, Ord)
instance (Num a, FromField a) => FromField (DefaultToZero a) where
    parseField "" = pure 0
    parseField s = DefaultToZero <$> parseField s
    
unwrapDTZ :: DefaultToZero a -> a
unwrapDTZ (DefaultToZero f) = f
  -}  
newtype SemicolonSeparatedList a = SemicolonSeparatedList [a]
instance FromField a => FromField (SemicolonSeparatedList a) where
    parseField "" = pure $ SemicolonSeparatedList []
    parseField f = SemicolonSeparatedList <$> (sequence $ map parseField $ BSC.split (';') f)
    
unwrapSSL :: SemicolonSeparatedList a -> [a]
unwrapSSL (SemicolonSeparatedList l) = l
    
instance FromField AdjectiveModifier where
    parseField f =
        let readField :: (Nameable a, Bounded a, Enum a) => Parser a
            readField = unwrapNameable <$> parseField f
      in
            (AMElement <$> readField)
        <|> (AMDerivedStat <$> readField)        
        <|> (AMBaseStat <$> readField)
        <|> (AMEfficiency <$> readField)
        <|> (AMMagnitude <$> readField)
    
newtype ColonSeparatedPair a b = ColonSeparatedPair (a, b)
instance (FromField a, FromField b) => FromField (ColonSeparatedPair a b) where
    parseField f = case BSC.split ':' f of
        [l, r] -> do
            l' <- parseField l
            r' <- parseField r
            pure $ ColonSeparatedPair (l', r')
        _ -> mzero

unwrapCSP :: ColonSeparatedPair a b -> (a, b)        
unwrapCSP (ColonSeparatedPair (a, b)) = (a, b)
    
newtype NameableWrapper a = NameableWrapper a deriving (Eq, Bounded)

unwrapNameable :: NameableWrapper a -> a
unwrapNameable (NameableWrapper a) = a
    
--data AdjectiveInteraction = AINoInteraction | AIExclusive AdjectiveId | AILikesElement Element | AIDislikesElement Element | AIHatesElement Element | AINeedsElement Element | AILikesWeapon WeaponClass | AIDislikesWeapon WeaponClass | AINeedsWeapon WeaponClass | AIHatesWeapon WeaponClass deriving (Eq, Show)

instance FromField ElementOrWeapon where
    parseField f = 
        let readField :: (Nameable b, Bounded b, Enum b) => Parser b
            readField = unwrapNameable <$> parseField f
      in    (EOWElement <$> readField)
        <|> (EOWWeapon <$> readField)

-- TODO: This is inefficient. it should probably just pattern match against the possible names? But that's a little repetitive for now, so atm it's defined in terms of Nameable.
instance (Nameable a, Bounded a, Enum a) => FromField (NameableWrapper a) where
    parseField f = let c = foldl' (\l r -> if name l == f then l else r) minBound [minBound..maxBound]
      in
        if (name c) == f
            then pure $ NameableWrapper c
            else mzero
    
readInteractions :: NamedRecord -> Parser [AdjectiveInteraction]
readInteractions m = let
    aiLikes = AILikes <:$> m .: "likes"
    aiDislikes = AIDislikes <:$> m .: "dislikes"
    aiHates = AIHates <:$> m .: "incompatible"
    aiNeeds = AINeeds <:$> m .: "needs"
    aiExcludes = (map (AIExclusive . AdjectiveId) <$> unwrapSSL <$> m .: "excludes")
  in
    concat <$> sequence [aiExcludes, aiHates, aiNeeds, aiLikes, aiDislikes]

(<:$>) :: (a -> b) -> Parser (SemicolonSeparatedList (a)) -> Parser [b]
(<:$>) constructor field = (map constructor) <$> unwrapSSL <$> field
infixr 7 <:$>

instance FromNamedRecord Adjective where
    parseNamedRecord m = let 
        so = m .: "sort order"
        aName = m .: "adjective"
        context = m .: "context"
        amStatMods = map unwrapCSP <$> unwrapSSL <$> m .: "modifiers"
        modifiers = filter (\a -> fst a /= 0) <$> amStatMods
        interactions = readInteractions m
        adjid = m .: "id"
        parseElementInteraction = \(f, am) -> if f == 0 then Nothing else case am of
            AMElement a -> Just a
            _ -> Nothing
        element = parseElementInteraction <$> foldl' (\l r -> if fst l > fst r then l else r) (0, AMElement minBound) <$> amStatMods
        weapon = (unwrapNameable <$>) <$> m .: "weapon class"
        level = m .: "ilvl"
        skillClassification = (unwrapNameable <$>) <$> m .: "classification"
        damageType = (unwrapNameable <$>) <$> m .: "damage type"
        effect = (unwrapNameable <$>) <$> m .: "effect"
      in
        Adjective <$> so <*> aName <*> context <*> modifiers <*> interactions <*> adjid <*> element <*> weapon <*> level <*> skillClassification <*> damageType <*> effect
    
instance Eq Adjective where
    (==) l r = adjId l == adjId r
    
instance Ord Adjective where
    compare l r = case compare (adjSortOrder l) (adjSortOrder r) of
        EQ -> compare (adjId l) (adjId r)
        LT -> LT
        GT -> GT
        
instance HasEIV (Float, AdjectiveModifier) Element where
    getEIV (v, AMElement e) = spliceAt (innerConstructor v) e mempty
    getEIV _ = mempty
    
instance HasEIV (Float, AdjectiveModifier) BaseStat where    
    getEIV (v, AMBaseStat e) = spliceAt (innerConstructor v) e mempty
    getEIV _ = mempty
    
instance HasEIV (Float, AdjectiveModifier) DerivedStat where    
    getEIV (v, AMDerivedStat e) = spliceAt (innerConstructor v) e mempty
    getEIV _ = mempty

type CanHasEIV e = (HasEIV (Float, AdjectiveModifier) e, EnumIndexedValues e)
    
instance CanHasEIV e => HasEIV Adjective e where
    getEIV = mconcat . (map getEIV) . adjModifiers
    
newtype AdjectiveList = AdjectiveList [Adjective] deriving (Monoid, Show)
unwrapAdjectiveList :: AdjectiveList -> [Adjective]
unwrapAdjectiveList (AdjectiveList al) = al
    
