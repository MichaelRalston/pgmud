{-# LANGUAGE ExistentialQuantification #-}

module PGMUD.Types.AdjectiveGenerator
    ( AdjectiveGenerator (..)
    ) where
    
import PGMUD.PGMUD
    
data AdjectiveGenerator m = (PGMUD m) => AdjectiveGenerator { selectAdjectiveType :: AdjectiveList -> m (Maybe (AdjectiveGenerator m), AdjectiveType) }

