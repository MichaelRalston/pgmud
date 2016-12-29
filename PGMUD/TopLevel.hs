{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module PGMUD.TopLevel
    ( module PGMUD.Skills
    , module PGMUD.Weapons
    , module PGMUD.Adjectives
    , getInitialState
    , HasElementalAffinities (..)
    , HasStatModifiers (..)
    ) where
    
import Prelude hiding (readFile)
import PGMUD.Skills
import PGMUD.Weapons
import PGMUD.Adjectives
import PGMUD.Types
import PGMUD.PGMUD
import PGMUD.Types.Adjective (AdjectiveType (..))
import PGMUD.Prelude

import qualified Data.Vector as Vector
import Data.Csv (decodeByName)
import Data.ByteString (readFile)
import qualified Data.Map as Map

loadAdjectiveList :: FilePath -> IO [Adjective]
loadAdjectiveList fname = do
    body <- readFile fname
    let decoded = decodeByName $ fromStrict body
    case decoded of
        Left err -> fail err
        Right adjs -> return $ Vector.toList $ snd adjs

getInitialState :: IO PGMUDState
getInitialState = do
    weaponClasses <- loadAdjectiveList "weapon_classes.csv"
    weaponElements <- loadAdjectiveList "weapon_elements.csv"
    weaponOrDamage <- loadAdjectiveList "skill_maybe_weapon.csv"
    invisibleDamageType <- loadAdjectiveList "skill_force_damage_type.csv"
    skillMagnitude <- loadAdjectiveList "skill_magnitude.csv"
    weaponQuality <- loadAdjectiveList "weapon_quality.csv"
    skillQuality <- loadAdjectiveList "skill_quality.csv"
    skillClassifications <- loadAdjectiveList "skill_classifications.csv"
    skillElements <- loadAdjectiveList "skill_elements.csv"
    let adjectiveTable = Map.fromList 
            [ (ATWeaponClass, weaponClasses)
            , (ATWeaponElement, weaponElements)
            , (ATSkillWeaponOrDamage, weaponOrDamage)
            , (ATInvisibleDamageType, invisibleDamageType)
            , (ATSkillMagnitude, skillMagnitude)
            , (ATChargeEffect, [])
            , (ATParryEffect, [])
            , (ATWeaponQuality, weaponQuality)
            , (ATSkillQuality, skillQuality)
            , (ATSkillClassification, skillClassifications)
            , (ATSkillElement, skillElements)
            ]
    return $ PGMUDState {..}
