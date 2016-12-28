{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module PGMUD.AdjectiveList
    ( weaponClass
    , effect
    , itemLevel
    , stringify
    ) where
    
import PGMUD.PGMUD 
import PGMUD.Prelude
import PGMUD.Adjectives
import PGMUD.Types.Adjective

import Safe (headMay)
import Data.Maybe (mapMaybe)
import Data.Text (intercalate)

weaponClass :: AdjectiveList -> Maybe WeaponClass
weaponClass (AdjectiveList al) = headMay $ mapMaybe adjWeapon $ al

effect :: AdjectiveList -> Maybe Effect
effect (AdjectiveList al) = undefined

itemLevel :: AdjectiveList -> ItemLevel
itemLevel (AdjectiveList al) = sum $ map adjLevel al

stringify :: AdjectiveList -> Text
stringify (AdjectiveList al) = let
    names = map adjName al
  in
    intercalate " " names