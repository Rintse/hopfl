-- Defines a function to turn expressions into trees which in turn 
-- leads to prettier printing using the pre-existing drawTree function

module Tools.Treeify where

import Syntax.Expression
import Semantics.Values
import Tools.VerbPrint

import Data.Tree
import Debug.Trace

class Treeish a where
    toTree :: a -> Tree String

instance Treeish Ident where
    toTree (Ident x id d) = Node (x ++ " (" ++ show id ++ ") " ++ "[" ++ show d ++"]") []

instance Treeish Environment where
    toTree (Env l) = Node "Substitution List" (map toTree l)

instance Treeish Assignment where
    toTree (Assign x t) = Node "Substitution" [ toTree x, toTree t ]

instance Treeish Exp where
    toTree exp = case exp of
        Single              -> Node "" []
        Var x               -> toTree x
        Val v               -> Node (show v)    []
        BVal b              -> Node (show b)    []
        Next e              -> Node "Next"      [ toTree e ]
        Unbox e             -> Node "Unbox"     [ toTree e]
        Box l e             -> Node "Box"       [ toTree l, toTree e]
        Prev l e            -> Node "Prev"      [ toTree l, toTree e ]
        In e                -> Node "In"        [ toTree e ]
        Min e               -> Node "Min"       [ toTree e ]
        Out e               -> Node "Out"       [ toTree e ]
        Fst e               -> Node "Fst"       [ toTree e ]
        Snd e               -> Node "Snd"       [ toTree e ]
        InL e               -> Node "InL"       [ toTree e ]
        InR e               -> Node "InR"       [ toTree e ]
        Norm e              -> Node "Norm"      [ toTree e ]
        Not e               -> Node "Not"       [ toTree e ]
        Print e             -> Node "Print"     [ toTree e ]
        App e1 e2           -> Node "App"       [ toTree e1, toTree e2 ]
        LApp e1 e2          -> Node "LApp"      [ toTree e1, toTree e2 ]
        Pair e1 e2          -> Node "Pair"      [ toTree e1, toTree e2 ]
        Ite b e1 e2         -> Node "Ite"       [ toTree b, toTree e1, toTree e2 ]
        Abstr x e           -> Node "λ"         [ toTree x, toTree e ]
        Rec f e             -> Node "Fix"       [ toTree f, toTree e ]
        Mul e1 e2           -> Node "Mul"       [ toTree e1, toTree e2 ]
        Pow e1 e2           -> Node "Pow"       [ toTree e1, toTree e2 ]
        Div e1 e2           -> Node "Div"       [ toTree e1, toTree e2 ]
        Add e1 e2           -> Node "Add"       [ toTree e1, toTree e2 ]
        Sub e1 e2           -> Node "Sub"       [ toTree e1, toTree e2 ]
        Lt e1 e2            -> Node "Lt"        [ toTree e1, toTree e2 ]
        Gt e1 e2            -> Node "Gt"        [ toTree e1, toTree e2 ]
        Leq e1 e2           -> Node "Leq"       [ toTree e1, toTree e2 ]
        Geq e1 e2           -> Node "Geq"       [ toTree e1, toTree e2 ]
        Eq e1 e2            -> Node "Eq"        [ toTree e1, toTree e2 ]
        And e1 e2           -> Node "And"       [ toTree e1, toTree e2 ]
        Or e1 e2            -> Node "Or"        [ toTree e1, toTree e2 ]
        Match e x1 e1 x2 e2 -> Node "Match"     [ toTree e,
                                                  toTree x1, toTree e1,
                                                  toTree x2, toTree e2 ]

isLast ( EIn ( EInR ( EPair v ( ENext ( EIn (EInL _) ) ) ) ) ) = True
isLast _ = False

printList l@(EIn ( EInR ( EPair v (ENext l2) ) ) ) = 
    init (drawTree (toTree v)) 
    ++ if isLast l then "" else ", " ++ printList l2
printList (EIn ( EInL _ ) ) = ""
printList o = trace (show o) ""

instance Treeish Value where
    toTree val = case val of
        VSingle     -> Node "" []
        VVal v      -> Node (show v)[ ]
        VBVal b     -> Node (show b)[ ]
        VPair t1 t2 -> Node "VPair" [ toTree t1, toTree t2 ]
        VIn t       -> Node "In"    [ toTree t ]
        VInL t      -> Node "InL"   [ toTree t ]
        VInR t      -> Node "InR"   [ toTree t ]
        VNext t     -> Node "Next"  [ toTree t ]
        VOut t      -> Node "Out"   [ toTree t ]
        VThunk t    -> Node "Thunk" [ toTree t ]
        VBox l t    -> Node "Box"   [ toTree t ]

        -- Evaluated results
        EPair v1 v2 -> Node "Pair"  [ toTree v1, toTree v2 ]
        EBox v      -> Node "Box"   [ toTree v ]
        EInL v      -> Node "InL"   [ toTree v ]
        EInR v      -> Node "InR"   [ toTree v ]
        EList (EBox l) -> Node ("[" ++ printList l ++ "]") []

treeValue :: Value -> String
treeValue v = drawTree $ toTree v

treeTerm :: Exp -> String
treeTerm e = drawTree $ toTree e

showProg :: Bool -> Exp -> IO ()
showProg v prog = do
    putStrV v $ "[Abstract Syntax]\n" ++ show prog ++ "\n"
    putStrV v $ "[Tree]\n" ++ treeTerm prog ++ "\n"

