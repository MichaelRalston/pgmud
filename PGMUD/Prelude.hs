module PGMUD.Prelude
    ( Text
    , ByteString
    , MonadPlus(..)
    , foldl'
    , IsString
    , Nameable (..)
    , mapSnd
    , (<>)
    , fromStrict
    , traceInline
    , traceTagged
    , traceShow
    , trace
    , traceWith
    , traceM
    , traceShowM
    , Alternative (..)
    , join
    , chooseRandom
    ) where
    
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.Text (Text)
import Control.Monad (MonadPlus(..), join)
import Data.List (foldl')
import Data.String (IsString)
import Data.Monoid ((<>))
import Control.Applicative (Alternative (..))
import System.Random (randomR, StdGen)

import Debug.Trace (traceShow, trace, traceM, traceShowM)

class Nameable a where
    name :: IsString s => a -> s

traceWith :: Show b => (a -> b) -> a -> a
traceWith tracer v = traceShow (tracer v) v
    
traceInline :: Show a => a -> a
traceInline v = traceShow v v

traceTagged :: Show a => String -> a -> a
traceTagged tag v = traceShow (tag, v) v
    
mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (a, b) = (a, f b)

chooseAtPos :: (Ord b, Num b) => [(b, a)] -> b -> [a]
chooseAtPos [] _ = []
chooseAtPos ((score, r):rest) pos = if score >= pos then [r] else chooseAtPos rest (pos-score)

chooseRandom :: [(Double, a)] -> StdGen -> ([a], StdGen)
chooseRandom scoredCandidates rng = let 
    scoreSum = foldl' (+) 0 $ map fst scoredCandidates
    (pos, rng') = randomR (0, scoreSum) rng
    chosen = chooseAtPos scoredCandidates pos
  in
    (chosen, rng')
