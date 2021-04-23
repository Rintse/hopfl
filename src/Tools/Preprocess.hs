module Tools.Preprocess where

import Syntax.Abs
import Semantics.Substitution

-- Perform a single definition substitution
defSub :: Exp -> Assignment -> Exp
defSub exp a@(Assign (Ident x) _ t) = case exp of
    Var (Ident y)           -> if y == x then t else exp
    Val v                   -> exp
    BVal b                  -> exp
    Next e                  -> Next $ defSub e a
    Prev (Env l) e ->
        let rl = map (\(Assign x o t) -> Assign x o $ defSub t a) l in
            Prev (Env rl) (defSub e a)
    PrevE e                 -> PrevE $ defSub e a
    PrevF e                 -> PrevF $ defSub e a
    Box (Env l) e ->
        let rl = map (\(Assign x o t) -> Assign x o $ defSub t a) l in
            Box (Env rl) (defSub e a)
    Unbox e                 -> Unbox $ defSub e a
    In e                    -> In $ defSub e a
    Out e                   -> Out $ defSub e a
    Fst e                   -> Fst $ defSub e a
    Snd e                   -> Snd $ defSub e a
    InL e                   -> InL $ defSub e a
    InR e                   -> InR $ defSub e a
    App e1 e2               -> App (defSub e1 a) (defSub e2 a)
    LApp e1 o e2            -> LApp (defSub e1 a) o (defSub e2 a)
    Mul e1 e2               -> Mul (defSub e1 a) (defSub e2 a)
    Div e1 e2               -> Div (defSub e1 a) (defSub e2 a)
    Add e1 e2               -> Add (defSub e1 a) (defSub e2 a)
    Sub e1 e2               -> Sub (defSub e1 a) (defSub e2 a)
    Eq e1 e2                -> Eq (defSub e1 a) (defSub e2 a)
    Lt e1 e2                -> Lt (defSub e1 a) (defSub e2 a)
    Gt e1 e2                -> Gt (defSub e1 a) (defSub e2 a)
    Leq e1 o e2             -> Leq (defSub e1 a) o (defSub e2 a)
    Geq e1 o e2             -> Geq (defSub e1 a) o (defSub e2 a)
    Not o e                 -> Not o (defSub e a)
    And e1 o e2             -> And (defSub e1 a) o (defSub e2 a)
    Or e1 o e2              -> Or (defSub e1 a) o (defSub e2 a)
    Pair e1 e2              -> Pair (defSub e1 a) (defSub e2 a)
    Norm e                  -> Norm $ defSub e a
    Ite e1 e2 e3            -> Ite (defSub e1 a) (defSub e2 a) (defSub e3 a)
    Abstr l x e             -> Abstr l x $ defSub e a
    Rec x e                 -> Rec x $ defSub e a
    Match e x _ e1 y o e2   -> Match (defSub e a) x o (defSub e1 a) y o (defSub e2 a)

-- Perform definition substitutions
handleDefs :: Prg -> Exp
handleDefs (DefProg (Env l) e) = foldl defSub e l
handleDefs (Prog e) = e
