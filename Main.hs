import PGMUD.TopLevel
import PGMUD.Prelude

import Control.Monad.Trans.State (evalStateT)

main :: IO ()
main = do
    s <- getInitialState
    w <- evalStateT (generateWeapon []) s
    traceShowM w