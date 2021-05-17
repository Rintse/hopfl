module Preprocess.Preprocess where

import Preprocess.Definitions
import Preprocess.Lists
import Preprocess.AnnotateVars
import Syntax.Raw.Abs as Raw
import qualified Syntax.Expression as Exp

-- Run all the preprocess steps
preprocess :: Raw.Prg -> Raw.Environment -> IO Exp.Exp
preprocess e env = do
    withDefinitions <- handleDefs   e
    let withLists   =  desugarLists withDefinitions
    return          $  annotateVars withLists
