{-# LANGUAGE RecordWildCards #-}

module PGMUD.Adjectives
    ( generateAdjective
    , buildAdjectiveList
    , simpleGen
    , composedGen
    , trivialGen
    ) where
    
import PGMUD.Prelude
import PGMUD.PGMUD
import PGMUD.Types.Adjective
import PGMUD.Types.AdjectiveGenerator
import Data.List (sortBy)
import Data.List.Unique (sortUniq)
import Data.Maybe (mapMaybe)

data AdjectiveWeight = AWValue Int | AWBanned | AWElements { awNeeds :: [Element], awHas :: [Element], awValue :: Int}

combineWeights :: AdjectiveWeight -> AdjectiveWeight -> AdjectiveWeight
combineWeights AWBanned _ = AWBanned
combineWeights _ AWBanned = AWBanned
combineWeights (AWValue i) (AWValue i') = AWValue (i+i')
combineWeights (AWElements n h i) (AWValue i') = AWElements n h (i+i')
combineWeights (AWValue i') (AWElements n h i) = AWElements n h (i+i')
combineWeights (AWElements n h i) (AWElements n' h' i') = AWElements (sortUniq (n++n')) (sortUniq (h++h')) (i+i')

applyInteraction :: AdjectiveInteraction -> Adjective -> AdjectiveWeight
applyInteraction (AINoInteraction) _ = AWValue 0
applyInteraction (AIExclusive aid) a = if aid == adjId a then AWBanned else AWValue 0
applyInteraction (AILikesElement e) a = if Just e == adjElem a then AWValue 1 else AWValue 0
applyInteraction (AIDislikesElement e) a = if Just e == adjElem a then AWValue (-1) else AWValue 0
applyInteraction (AIHatesElement e) a = if Just e == adjElem a then AWBanned else AWValue 0
applyInteraction (AINeedsElement e) a = if Just e == adjElem a then AWElements [e] [e] 0 else AWElements [e] [] 0
applyInteraction (AILikesWeapon e) a = if Just e == adjWeapon a then AWValue 1 else AWValue 0
applyInteraction (AIDislikesWeapon e) a = if Just e == adjWeapon a then AWValue (-1) else AWValue 0

assignWeights :: [Adjective] -> Adjective -> (AdjectiveWeight, Adjective)
assignWeights modifiers newAdjective = let
    weight = foldl' combineWeights (AWValue 0) interactions
    interactions = [applyInteraction processInteraction modifier | processInteraction <- adjInteractions newAdjective, modifier <- modifiers]    
  in
    (weight, newAdjective)
    
convertWeightToScore :: (AdjectiveWeight, Adjective) -> Maybe (Double, Adjective)
convertWeightToScore (AWBanned, _) = Nothing
convertWeightToScore ((AWValue v), adj) = Just (5 ** (fromInteger $ toInteger v), adj) -- TODO: reevaluate the effect of this on probabilities.
convertWeightToScore (AWElements{..}, adj) = if awNeeds == awHas then convertWeightToScore ((AWValue awValue), adj) else Nothing
    
generateAdjective :: [Adjective] -> [Adjective] -> StdGen -> ([Adjective], StdGen)
generateAdjective sourceList preChosen rng = let 
    candidates = map (assignWeights preChosen) sourceList
    scoredCandidates = mapMaybe convertWeightToScore candidates
    (chosen, rng') = chooseRandom scoredCandidates rng
  in
    (preChosen ++ chosen, rng')

generateAdjectives :: PGMUD m => AdjectiveGenerator m -> AdjectiveList -> m AdjectiveList
generateAdjectives (AdjectiveGenerator{..}) context = do
    (gen', t) <- selectAdjectiveType context
    source <- getAdjectiveList t
    list' <- withRandom $ generateAdjective source $ unwrapAdjectiveList context
    let list = AdjectiveList list'
    case gen' of
        Nothing -> return list
        Just gen'' -> generateAdjectives gen'' list
    
simpleGen :: PGMUD m => [AdjectiveType] -> AdjectiveGenerator m
simpleGen [] = error "Cannot generate an empty list"
simpleGen (at:[]) = AdjectiveGenerator $ const $ return (Nothing, at)
simpleGen (at:rest) = AdjectiveGenerator $ const $ return (Just $ simpleGen rest, at)
    
buildAdjectiveList :: PGMUD m => [AdjectiveType] -> AdjectiveList -> m AdjectiveList
buildAdjectiveList ats context = do
    unsortedAdjectives <- generateAdjectives (simpleGen ats) context
    return $ AdjectiveList $ sortBy (\l r -> adjSortOrder l `compare` adjSortOrder r) $ unwrapAdjectiveList unsortedAdjectives
    
composedGen :: PGMUD m => AdjectiveGenerator m -> AdjectiveGenerator m -> AdjectiveGenerator m
composedGen (AdjectiveGenerator{..}) r = AdjectiveGenerator $ \context -> do
    (l, t) <- selectAdjectiveType context
    case l of
        Nothing -> return (Just r, t)
        Just l' -> return (Just $ composedGen l' r, t)
        
trivialGen :: PGMUD m => m (Maybe (AdjectiveGenerator m), AdjectiveType) -> AdjectiveGenerator m
trivialGen = AdjectiveGenerator . const