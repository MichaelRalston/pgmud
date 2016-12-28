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
import qualified PGMUD.AdjectiveList as AdjectiveList
    
generateWeapon :: PGMUD m => AdjectiveList -> m Weapon
generateWeapon configuration = do
    adjectives <- buildAdjectiveList [ATWeaponClass, ATWeaponElement, ATWeaponElement] configuration
    return $ Weapon adjectives
    
weaponClass :: Weapon -> WeaponClass
weaponClass w = case AdjectiveList.weaponClass $ weaponAdjectives w of
    Just wc -> wc
    Nothing -> error "Weapons must have a weapon class."

weaponEffect :: Weapon -> Effect
weaponEffect w = case AdjectiveList.effect $ weaponAdjectives w of
    Just e -> e
    Nothing -> error "Weapons must have an effect."

weaponLevel :: Weapon -> ItemLevel
weaponLevel = AdjectiveList.itemLevel . weaponAdjectives

weaponName :: Weapon -> Text
weaponName = AdjectiveList.stringify . weaponAdjectives
