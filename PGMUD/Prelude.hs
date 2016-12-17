module PGMUD.Prelude
    ( Text
    , ByteString
    , MonadPlus(..)
    , foldl'
    , IsString
    , Nameable (..)
    , mapSnd
    ) where
    
import Data.ByteString (ByteString)
import Data.Text (Text)
import Control.Monad (MonadPlus(..))
import Data.List (foldl')
import Data.String (IsString)

class Nameable a where
    name :: IsString s => a -> s

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (a, b) = (a, f b)

