{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module PGMUD.Types.Effect 
    ( Effect (..)
    )where

import PGMUD.Prelude

data Effect = Effect deriving (Show, Bounded, Enum)

instance Nameable Effect where
    name Effect = "effect"