{-# LANGUAGE RecordWildCards #-}

module PGMUD.Adjectives
    ( generateAdjective
    , buildAdjectiveList
    ) where
    
import PGMUD.Prelude
import PGMUD.PGMUD
import PGMUD.Types.Adjective
import Control.Monad (foldM)
import Data.List.Unique (sortUniq)
import Data.Maybe (mapMaybe)
import System.Random (randomR)

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

chooseAtPos :: (Ord b, Num b) => [(b, a)] -> b -> [a]
chooseAtPos [] _ = []
chooseAtPos ((score, r):rest) pos = if score <= pos then [r] else chooseAtPos rest (pos-score)
    
convertWeightToScore :: (AdjectiveWeight, Adjective) -> Maybe (Double, Adjective)
convertWeightToScore (AWBanned, _) = Nothing
convertWeightToScore ((AWValue v), adj) = Just (5 ** (fromInteger $ toInteger v), adj) -- TODO: reevaluate the effect of this on probabilities.
convertWeightToScore (AWElements{..}, adj) = if awNeeds == awHas then convertWeightToScore ((AWValue awValue), adj) else Nothing
    
generateAdjective :: [Adjective] -> [Adjective] -> StdGen -> ([Adjective], StdGen)
generateAdjective sourceList preChosen rng = let 
    candidates = map (assignWeights preChosen) sourceList
    scoredCandidates = mapMaybe convertWeightToScore candidates
    scoreSum = foldl' (+) 0 $ map fst scoredCandidates
    (pos, rng') = randomR (0, scoreSum) rng
    chosen = chooseAtPos scoredCandidates pos
  in
    (preChosen ++ chosen, rng')

-- TODO: Some kind of templating system, to make it easier to select different adjective templates.
buildAdjectiveList :: PGMUD m => [AdjectiveType] -> [Adjective] -> m [Adjective]
buildAdjectiveList ats context = do
    sources <- mapM getAdjectiveList ats
    (flip $ foldM (\l r -> withRandom $ generateAdjective l r)) sources context