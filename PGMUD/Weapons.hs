{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module PGMUD.Weapons
    ( generateWeapon
    , weaponClass
    , weaponEffect
    , weaponLevel
    , weaponName
    ) where
    
import PGMUD.PGMUD 
import PGMUD.Types.Weapon (Weapon(..))
import PGMUD.Prelude
import PGMUD.Adjectives
import PGMUD.Types.Adjective (adjWeapon, adjLevel, adjName)
import Data.Text (intercalate)

import Data.Maybe (mapMaybe)
    
generateWeapon :: PGMUD m => [Adjective] -> m Weapon
generateWeapon configuration = do
    adjectives <- buildAdjectiveList [ATWeaponClass, ATWeaponElement, ATWeaponElement] configuration
    return $ Weapon adjectives
    
weaponClass :: Weapon -> WeaponClass
weaponClass = head . (mapMaybe adjWeapon) . weaponAdjectives

weaponEffect :: Weapon -> Effect
weaponEffect = undefined

weaponLevel :: Weapon -> ItemLevel
weaponLevel = sum . (map adjLevel) . weaponAdjectives

weaponName :: Weapon -> Text
weaponName w = let
    names = map adjName $ weaponAdjectives w
  in
    intercalate " " names