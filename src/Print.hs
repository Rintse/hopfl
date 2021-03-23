module Print where
import Syntax.Abs
import Values

import Control.Monad.Reader
import Control.Applicative

printTerm :: Exp -> String
printTerm exp = runReader (printT exp) "" ++ "\n"

printValue :: Value -> String
printValue val = ( case val of
    VVal v      -> show val
    VBVal b     -> show val
    VPair t1 t2 -> "VPair:\n" ++ printTerm t1 ++ printTerm t2
    VIn t       -> "VIn:\n" ++ printTerm t
    VInL t      -> "VInL:\n" ++ printTerm t
    VInR t      -> "VInR:\n" ++ printTerm t
    VNext t     -> "VNext:\n" ++ printTerm t
    VOut t      -> "VOut:\n" ++ printTerm t
    VThunk t    -> "VThunk: \n" ++ printTerm t
    ) ++ "\n"

indent :: Int -> String
indent n = replicate (n*4) ' '

putTerm :: String -> Int -> String
putTerm s i = indent i ++ s ++ "\n"

putChild :: String -> String
putChild prefix = prefix ++ "+---"

cat3 :: String -> String -> String -> String
cat3 s1 s2 s3 = s1 ++ s2 ++ s3

printT1 :: String -> Exp -> Reader String String
printT1 s e = liftA3 cat3
    (return (s ++ "\n"))
    (asks putChild)
    (local (++"|   ") (printT e))

printT2 :: String -> Exp -> Exp -> Reader String String
printT2 s e1 e2 = liftA3 cat3 (printT1 s e1) 
    (asks putChild) (local (++"|   ") (printT e2))

printT3 :: String -> Exp -> Exp -> Exp -> Reader String String
printT3 s e1 e2 e3 = liftA3 cat3 (printT2 s e1 e2) 
    (asks putChild) (local (++"|   ") (printT e3))

printT :: Exp -> Reader String String
printT exp = case exp of
    Var (Ident x)       -> return $ x ++ "\n"
    Val v               -> return $ show v ++ "\n"
    BVal b              -> return $ show b ++ "\n"
    Next e              -> printT1 "Next" e
    In e                -> printT1 "In" e
    Out e               -> printT1 "Out" e
    Fst e               -> printT1 "Fst" e
    Snd e               -> printT1 "Snd" e
    InL e               -> printT1 "InL" e
    InR e               -> printT1 "InR" e
    Norm e              -> printT1 "Norm" e
    Not _ e             -> printT1 "Not" e
    App e1 e2           -> printT2 "App" e1 e2
    LApp e1 _ e2        -> printT2 "LApp" e1 e2
    Pair e1 e2          -> printT2 "Pair" e1 e2
    Ite b e1 e2         -> printT3 "Ite" b e1 e2
    Abstr l (Ident x) e -> printT1 ("Lambda " ++ x) e
    Rec (Ident x) e     -> printT1 "Fix" e
    Mul e1 e2           -> printT2 "Mul" e1 e2
    Div e1 e2           -> printT2 "Div" e1 e2
    Add e1 e2           -> printT2 "Add" e1 e2
    Sub e1 e2           -> printT2 "Sub" e1 e2
    Lt e1 e2            -> printT2 "Lt" e1 e2
    Gt e1 e2            -> printT2 "Gt" e1 e2
    Leq e1 _ e2         -> printT2 "Leq" e1 e2
    Geq e1 _ e2         -> printT2 "Geq" e1 e2
    Eq e1 e2            -> printT2 "Eq" e1 e2
    And e1 _ e2         -> printT2 "And" e1 e2
    Or e1 _ e2          -> printT2 "Or" e1 e2
    Match e (Ident x1) e1 (Ident x2) e2 -> 
        printT2 ("Match (inL=" ++ x1 ++ ", inR=" ++ x2 ++")") e1 e2
