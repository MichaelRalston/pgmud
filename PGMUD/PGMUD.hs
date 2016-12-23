{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module PGMUD.PGMUD
    ( PGMUD
    , StdGen
    , HasAdjectives(..)
    , WithRandom(..)
    , PGMUDState (..)
    , module PGMUD.Types
    ) where
    
import PGMUD.Types
import System.Random (StdGen, getStdRandom)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Map.Strict (Map, (!))
import Control.Monad.Trans.State.Lazy (StateT, get)

data PGMUDState = PGMUDState { 
        adjectiveTable :: Map AdjectiveType [Adjective]
    } deriving (Show)

class HasAdjectives m where
    getAdjectiveList :: AdjectiveType -> m [Adjective]

class WithRandom m where
    withRandom :: (StdGen -> (a, StdGen)) -> m a
    
class (Monad m, WithRandom m, HasAdjectives m) => PGMUD m

instance MonadIO m => WithRandom m where
    withRandom = liftIO . getStdRandom
    
instance Monad m => HasAdjectives (StateT PGMUDState m) where
    getAdjectiveList at = do
        s <- get
        let table = adjectiveTable s
        return $ table ! at
        
instance PGMUD (StateT PGMUDState IO)