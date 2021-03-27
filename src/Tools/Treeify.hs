-- Defines a function to turn expressions into trees which in turn 
-- leads to prettier printing using the pre-existing drawTree function

module Tools.Treeify where

import Syntax.IdAbs
import Semantics.Values
import Tools.VerbPrint

import Data.Tree

class Treeish a where
  toTree :: a -> Tree String

instance Treeish Exp where
    toTree exp = case exp of
        Var (Ident x _ _)   -> Node x           []
        Val v               -> Node (show v)    []
        BVal b              -> Node (show b)    []
        Next e              -> Node "Next"      [ toTree e ]
        In e                -> Node "In"        [ toTree e ]
        Out e               -> Node "Out"       [ toTree e ]
        Fst e               -> Node "Fst"       [ toTree e ]
        Snd e               -> Node "Snd"       [ toTree e ]
        InL e               -> Node "InL"       [ toTree e ]
        InR e               -> Node "InR"       [ toTree e ]
        Norm e              -> Node "Norm"      [ toTree e ]
        Not _ e             -> Node "Not"       [ toTree e ]
        App e1 e2           -> Node "App"       [ toTree e1, toTree e2 ]
        LApp e1 _ e2        -> Node "LApp"      [ toTree e1, toTree e2 ]
        Pair e1 e2          -> Node "Pair"      [ toTree e1, toTree e2 ]
        Ite b e1 e2         -> Node "Ite"       [toTree b, toTree e1, toTree e2 ]
        Abstr l (Ident x _ _) e -> Node ("λ "++x)   [ toTree e ]
        Rec (Ident x _ _) e -> Node "Fix"       [ toTree e ]
        Mul e1 e2           -> Node "Mul"       [ toTree e1, toTree e2 ]
        Div e1 e2           -> Node "Div"       [ toTree e1, toTree e2 ]
        Add e1 e2           -> Node "Add"       [ toTree e1, toTree e2 ]
        Sub e1 e2           -> Node "Sub"       [ toTree e1, toTree e2 ]
        Lt e1 e2            -> Node "Lt"        [ toTree e1, toTree e2 ]
        Gt e1 e2            -> Node "Gt"        [ toTree e1, toTree e2 ]
        Leq e1 _ e2         -> Node "Leq"       [ toTree e1, toTree e2 ]
        Geq e1 _ e2         -> Node "Geq"       [ toTree e1, toTree e2 ]
        Eq e1 e2            -> Node "Eq"        [ toTree e1, toTree e2 ]
        And e1 _ e2         -> Node "And"       [ toTree e1, toTree e2 ]
        Or e1 _ e2          -> Node "Or"        [ toTree e1, toTree e2 ]
        Match e (Ident x1 _ _) e1 (Ident x2 _ _) e2 ->
            Node ("Match (inL=" ++ x1 ++ ", inR=" ++ x2 ++")") [toTree e1, toTree e2]

treeTerm :: Exp -> String
treeTerm e = drawTree $ toTree e 

treeValue :: Value -> String
treeValue val = ( case val of
    VVal v      -> show val
    VBVal b     -> show val
    VPair t1 t2 -> "VPair:\n" ++ treeTerm t1 ++ treeTerm t2
    VIn t       -> "VIn:\n" ++ treeTerm t
    VInL t      -> "VInL:\n" ++ treeTerm t
    VInR t      -> "VInR:\n" ++ treeTerm t
    VNext t     -> "VNext:\n" ++ treeTerm t
    VOut t      -> "VOut:\n" ++ treeTerm t
    VThunk t    -> "VThunk: \n" ++ treeTerm t
    ) ++ "\n"

showProg :: Bool -> Exp -> IO ()
showProg v prog = do
    putStrV v $ "[Abstract Syntax]\n" ++ show prog ++ "\n"
    putStrV v $ "[Tree]\n" ++ treeTerm prog ++ "\n"

