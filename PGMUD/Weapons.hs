{-# LANGUAGE ScopedTypeVariables #-}

module PGMUD.Weapons
    ( generateWeapon
    , weaponClass
    , weaponEffect
    , weaponElements
    , weaponLevel
    , weaponName
    ) where
    
import PGMUD.PGMUD 
import PGMUD.Types.Weapon (Weapon(..))
import PGMUD.Prelude
import PGMUD.Adjectives
import PGMUD.Types.Adjective (adjWeapon)

import Data.Maybe (mapMaybe)
    
generateWeapon :: PGMUD m => [Adjective] -> m Weapon
generateWeapon configuration = do
    adjectives <- buildAdjectiveList [ATWeaponClass, ATWeaponElement, ATWeaponElement] configuration
    return $ Weapon adjectives
    
weaponClass :: Weapon -> WeaponClass
weaponClass (Weapon adjs) = head $ mapMaybe adjWeapon adjs

weaponEffect :: Weapon -> Effect
weaponEffect = undefined

weaponElements :: Weapon -> ElementalAffinities
weaponElements = undefined

weaponLevel :: Weapon -> ItemLevel
weaponLevel = undefined

weaponName :: Weapon -> ByteString
weaponName = undefined