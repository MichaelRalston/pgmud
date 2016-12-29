{-# LANGUAGE RecordWildCards #-}

import PGMUD.TopLevel
import PGMUD.Prelude

import PGMUD.Types.Skill

import Control.Monad.Trans.State (evalStateT)

main :: IO ()
main = do
    s <- getInitialState
    w <- evalStateT (generateWeapon mempty) s
    traceShowM ""
    traceShowM ("weapon", w)
    traceShowM ""
    traceShowM ("name", weaponName w)
    traceShowM ("class", weaponClass w)
    traceShowM ("level", weaponLevel w)
    traceShowM ("elemental affinities", elementalAffinities w)
    traceShowM ("base stats", baseStatModifiers w)
    traceShowM ("derived stats", derivedStatModifiers w)
    traceShowM ""
    traceShowM ""
    skill <- evalStateT (Skill <$> generateAdjectives skillGen mempty) s
    traceShowM ("skill", skill)
    traceShowM ""
    traceShowM ("name", skillName skill)
    
