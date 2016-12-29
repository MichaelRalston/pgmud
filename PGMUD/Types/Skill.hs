module PGMUD.Types.Skill 
    ( Skill (..)
    ) where

import PGMUD.Types.Adjective
    
data Skill = Skill { skillAdjectives :: AdjectiveList } deriving (Show)

