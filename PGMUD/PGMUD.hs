module PGMUD.PGMUD
    ( PGMUD
    , StdGen
    , HasAdjectives(..)
    , WithRandom(..)
    , module PGMUD.Types
    ) where
    
import PGMUD.Types
import System.Random (StdGen)

class HasAdjectives m where
    adjWeaponTypes :: m [Adjective]
    adjWeaponElements :: m [Adjective]
    adjSkillElements :: m [Adjective]
    adjQuality :: m [Adjective]
    adjEffects :: m [Adjective]
    adjSkillsWeaponType :: m [Adjective]
    adjSkillCategory :: m [Adjective]
    adjSkillEffects :: m [Adjective]

class WithRandom m where
    withRandom :: (StdGen -> (a, StdGen)) -> m a
    
class (Monad m, WithRandom m, HasAdjectives m) => PGMUD m