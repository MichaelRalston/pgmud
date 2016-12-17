module PGMUD.Types.Skill 
    ( Skill(..)
    ) where

import PGMUD.Types.Adjective
    
data Skill = Skill [Adjective]
