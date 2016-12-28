module PGMUD.Skills
    ( skillElement
    , skillCost
    , skillGen
    ) where

import PGMUD.PGMUD 
import PGMUD.Types.Adjective
import PGMUD.Types.AdjectiveGenerator
import qualified PGMUD.AdjectiveList as AdjectiveList
    
skillElement :: Skill -> Element
skillElement = undefined

skillCost :: Skill -> WeaponClass -> APAmount
skillCost = undefined

-- data AdjectiveGenerator m = (PGMUD m) => AdjectiveGenerator { selectAdjectiveType :: AdjectiveList -> m (Maybe (AdjectiveGenerator m), AdjectiveType) }
-- data AdjectiveType = ATWeaponClass | ATWeaponElement | ATSkillWeapon | ATSkillWeaponGroup | ATWeaponQuality | ATSkillQuality | ATSkillClassification | ATSkillElement deriving (Show, Eq, Ord)
skillGen :: PGMUD m => AdjectiveList -> m (Maybe (AdjectiveGenerator m), AdjectiveType)
skillGen al = let
    hasElement = case AdjectiveList.element al of
        Nothing -> False
        Just _ -> True
    elementGen = return $ (Just $ AdjectiveGenerator skillGen, ATSkillElement)
    attackGen = if hasElement
        then undefined
        else elementGen
    defenseGen = undefined
    chargeGen = undefined
    parryGen = undefined
    healGen = undefined
  in
    case AdjectiveList.skillClassification al of
        Nothing -> return $ (Just $ AdjectiveGenerator skillGen, ATSkillClassification)
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
