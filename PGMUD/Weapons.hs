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
    
generateWeapon :: PGMUD m => [Adjective] -> m Weapon
generateWeapon configuration = do
    typeAdjs <- adjWeaponTypes
    elemAdjs <- adjWeaponElements
    adjectives <- buildAdjectiveList [typeAdjs, elemAdjs, elemAdjs] configuration
    return $ Weapon adjectives
    
weaponClass :: Weapon -> WeaponClass
weaponClass = undefined

weaponEffect :: Weapon -> Effect
weaponEffect = undefined

weaponElements :: Weapon -> ElementalAffinities
weaponElements = undefined

weaponLevel :: Weapon -> ItemLevel
weaponLevel = undefined

weaponName :: Weapon -> ByteString
weaponName = undefined