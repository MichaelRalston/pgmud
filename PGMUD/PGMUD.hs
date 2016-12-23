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
    getAdjectiveList :: AdjectiveType -> m [Adjective]

class WithRandom m where
    withRandom :: (StdGen -> (a, StdGen)) -> m a
    
class (Monad m, WithRandom m, HasAdjectives m) => PGMUD m