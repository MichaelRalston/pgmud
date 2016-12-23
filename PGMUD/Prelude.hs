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
    ) where
    
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.Text (Text)
import Control.Monad (MonadPlus(..))
import Data.List (foldl')
import Data.String (IsString)
import Data.Monoid ((<>))

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

