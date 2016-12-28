{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module PGMUD.AdjectiveList
    ( weaponClass
    , effect
    , itemLevel
    , stringify
    , skillClassification
    , element
    , damageCategory
    ) where
    
import PGMUD.PGMUD 
import PGMUD.Prelude
import PGMUD.Types.Adjective

import Safe (headMay)
import Data.Maybe (mapMaybe)
import Data.Text (intercalate)

element :: AdjectiveList -> Maybe Element
element (AdjectiveList al) = headMay $ mapMaybe adjElem al

damageCategory :: AdjectiveList -> Maybe DamageType
damageCategory (AdjectiveList al) = headMay $ mapMaybe adjDamageType al

weaponClass :: AdjectiveList -> Maybe WeaponClass
weaponClass (AdjectiveList al) = headMay $ mapMaybe adjWeapon al

skillClassification :: AdjectiveList -> Maybe SkillClassification
skillClassification (AdjectiveList al) = headMay $ mapMaybe adjClassification al

effect :: AdjectiveList -> Maybe Effect
effect (AdjectiveList _) = undefined

itemLevel :: AdjectiveList -> ItemLevel
itemLevel (AdjectiveList al) = sum $ map adjLevel al

stringify :: AdjectiveList -> Text
stringify (AdjectiveList al) = let
    names = map adjName al
  in
    intercalate " " names