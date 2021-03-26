module Tools.VerbPrint where 

import Control.Monad (when)

putStrV :: Bool -> String -> IO ()
putStrV v s = when v $ putStrLn s
