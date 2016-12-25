import PGMUD.TopLevel
import PGMUD.Prelude

import Control.Monad.Trans.State (evalStateT)

main :: IO ()
main = do
    s <- getInitialState
    w <- evalStateT (generateWeapon []) s
    putStrLn "\n"
    traceShowM ("weapon", w)
    putStrLn "\n"
    traceShowM ("name", weaponName w)
    traceShowM ("class", weaponClass w)
    traceShowM ("level", weaponLevel w)
    traceShowM ("elemental affinities", elementalAffinities w)
    traceShowM ("base stats", baseStatModifiers w)
    traceShowM ("derived stats", derivedStatModifiers w)