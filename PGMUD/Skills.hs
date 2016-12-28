{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module PGMUD.Skills
    ( skillElement
    , skillCost
    , skillGen
    ) where

import PGMUD.PGMUD 
import PGMUD.Prelude
import PGMUD.Types.Adjective
import PGMUD.Types.AdjectiveGenerator
import PGMUD.Types.Skill
import PGMUD.Adjectives
import qualified PGMUD.AdjectiveList as AdjectiveList
    
skillElement :: Skill -> Maybe Element
skillElement = AdjectiveList.element . skillAdjectives

skillCost :: Skill -> WeaponClass -> APAmount
skillCost = undefined

-- data AdjectiveGenerator m = (PGMUD m) => AdjectiveGenerator { selectAdjectiveType :: AdjectiveList -> m (Maybe (AdjectiveGenerator m), AdjectiveType) }
--data AdjectiveType = ATWeaponClass | ATWeaponElement | ATSkillWeaponOrDamage | ATInvisibleDamageType | ATChargeEffect | ATParryEffect | ATSkillMagnitude | ATWeaponQuality | ATSkillQuality | ATSkillClassification | ATSkillElement deriving (Show, Eq, Ord)
skillGen :: PGMUD m => AdjectiveGenerator m
skillGen = AdjectiveGenerator $ \context -> do
    let
        hasElement = case AdjectiveList.element context of
            Nothing -> False
            Just _ -> True
        hasDamageCategory = undefined
        hasWeaponType = case AdjectiveList.weaponClass context of
            Nothing -> False
            Just _ -> True
        elementGen = return $ (Just skillGen, ATSkillElement)
        attackGen = if hasElement
            then if hasDamageCategory
                then return $ (Nothing, ATSkillMagnitude)
                else if hasWeaponType
                    then return $ (Just skillGen, ATInvisibleDamageType)
                    else return $ (Just skillGen, ATSkillWeaponOrDamage)
            else elementGen
        defenseGen = return $ (Nothing, ATSkillMagnitude)
        parryAndChargeGen = trivialGen $ return $ (Nothing, ATSkillMagnitude)
        chargeGen = return $ (Just parryAndChargeGen, ATChargeEffect)
        parryGen = do
            [parryOrCharge] <- withRandom $ chooseRandom [(0.2, ATChargeEffect), (0.8, ATParryEffect)]
            return $ (Just parryAndChargeGen, parryOrCharge)
        healGen = return $ (Nothing, ATSkillMagnitude)
        selectQuality = chooseRandom [(0.15, [ATSkillQuality, ATSkillQuality, ATSkillQuality]), (0.35, [ATSkillQuality, ATSkillQuality]), (0.5, [ATSkillQuality])]
        qualityGen = AdjectiveGenerator $ \qualityContext -> do
            qualityChoice <- concat <$> withRandom selectQuality
            let (AdjectiveGenerator {..}) = composedGen (simpleGen qualityChoice) skillGen
            selectAdjectiveType qualityContext            
    case AdjectiveList.skillClassification context of
        Nothing -> return $ (Just qualityGen, ATSkillClassification)
        Just SCPiercing -> attackGen
        Just SCHoming -> attackGen
        Just SCStrike -> attackGen
        Just SCCharge -> chargeGen
        Just SCBlock -> defenseGen
        Just SCDodge -> defenseGen
        Just SCFlurry -> attackGen
        Just SCSmash -> attackGen
        Just SCBerserk -> attackGen
        Just SCParry -> parryGen
        Just SCHeal -> healGen
