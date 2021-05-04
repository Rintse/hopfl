
module Semantics.Builtins where

import Syntax.Raw.Abs

builtins :: [Assignment]
builtins =
    [Assign (Ident "head_g") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "s") (Fst (Out (Unbox (Var (Ident "s")))))),Assign (Ident "tail_g") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "s") (BoxI (PrevI (Snd (Out (Unbox (Var (Ident "s")))))))),Assign (Ident "idx_g") (TSub "\\8592") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "idx") (Abstr (Lam "\\955") (Ident "s") (Ite (Eq (Var (Ident "idx")) (IVal 0)) (App (Abstr (Lam "\\955") (Ident "s") (Fst (Out (Unbox (Var (Ident "s")))))) (Var (Ident "s"))) (PrevI (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Sub (Var (Ident "idx")) (IVal 1)))) (TLApp "\\8857") (Next (App (Abstr (Lam "\\955") (Ident "s") (BoxI (PrevI (Snd (Out (Unbox (Var (Ident "s")))))))) (Var (Ident "s"))))))))))] ++
        [Assign (Ident "exp") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (Pow (DVal 2.71828) (Var (Ident "x")))),Assign (Ident "min") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (Abstr (Lam "\\955") (Ident "y") (Ite (Lt (Var (Ident "x")) (Var (Ident "y"))) (Var (Ident "x")) (Var (Ident "y"))))),Assign (Ident "max") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (Abstr (Lam "\\955") (Ident "y") (Ite (Gt (Var (Ident "x")) (Var (Ident "y"))) (Var (Ident "x")) (Var (Ident "y"))))),Assign (Ident "add") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (Abstr (Lam "\\955") (Ident "y") (Add (Var (Ident "x")) (Var (Ident "y"))))),Assign (Ident "subtract") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (Abstr (Lam "\\955") (Ident "y") (Sub (Var (Ident "x")) (Var (Ident "y"))))),Assign (Ident "divide") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (Abstr (Lam "\\955") (Ident "y") (Div (Var (Ident "x")) (Var (Ident "y"))))),Assign (Ident "multiply") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (Abstr (Lam "\\955") (Ident "y") (Mul (Var (Ident "x")) (Var (Ident "y"))))),Assign (Ident "power") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (Abstr (Lam "\\955") (Ident "y") (Pow (Var (Ident "x")) (Var (Ident "y"))))),Assign (Ident "xor") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "p") (Abstr (Lam "\\955") (Ident "q") (And (Or (Var (Ident "p")) (Disj "\\8744") (Var (Ident "q"))) (Conj "\\8743") (Not (TNot "\\172") (And (Var (Ident "p")) (Conj "\\8743") (Var (Ident "q"))))))),Assign (Ident "implies") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "p") (Abstr (Lam "\\955") (Ident "q") (Or (Not (TNot "\\172") (Var (Ident "p"))) (Disj "\\8744") (Var (Ident "q")))))] ++
        [Assign (Ident "cons") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "e") (Abstr (Lam "\\955") (Ident "l") (InR (Pair (Var (Ident "e")) (Var (Ident "l")))))),Assign (Ident "head") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "l") (Match (Var (Ident "l")) (Ident "x") (TMatch "\\8594") (Var (Ident "x")) (Ident "y") (TMatch "\\8594") (Fst (Var (Ident "y"))))),Assign (Ident "tail") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "l") (Match (Var (Ident "l")) (Ident "x") (TMatch "\\8594") (Var (Ident "x")) (Ident "y") (TMatch "\\8594") (Snd (Var (Ident "y"))))),Assign (Ident "null") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "l") (Match (Var (Ident "l")) (Ident "x") (TMatch "\\8594") (BVal BTrue) (Ident "y") (TMatch "\\8594") (BVal BFalse))),Assign (Ident "length") (TSub "\\8592") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "l") (Match (Var (Ident "l")) (Ident "x") (TMatch "\\8594") (IVal 0) (Ident "y") (TMatch "\\8594") (Add (IVal 1) (PrevI (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Snd (Var (Ident "y")))))))))),Assign (Ident "index") (TSub "\\8592") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "idx") (Abstr (Lam "\\955") (Ident "list") (Ite (Eq (Var (Ident "idx")) (IVal 0)) (App (Abstr (Lam "\\955") (Ident "l") (Match (Var (Ident "l")) (Ident "x") (TMatch "\\8594") (Var (Ident "x")) (Ident "y") (TMatch "\\8594") (Fst (Var (Ident "y"))))) (Var (Ident "list"))) (PrevI (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Sub (Var (Ident "idx")) (IVal 1)))) (TLApp "\\8857") (Next (App (Abstr (Lam "\\955") (Ident "l") (Match (Var (Ident "l")) (Ident "x") (TMatch "\\8594") (Var (Ident "x")) (Ident "y") (TMatch "\\8594") (Snd (Var (Ident "y"))))) (Var (Ident "list")))))))))),Assign (Ident "append") (TSub "\\8592") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "l1") (Abstr (Lam "\\955") (Ident "l2") (Match (Var (Ident "l1")) (Ident "end") (TMatch "\\8594") (Var (Ident "l2")) (Ident "lst") (TMatch "\\8594") (App (App (Abstr (Lam "\\955") (Ident "e") (Abstr (Lam "\\955") (Ident "l") (InR (Pair (Var (Ident "e")) (Var (Ident "l")))))) (Fst (Var (Ident "lst")))) (PrevI (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Snd (Var (Ident "lst"))))) (TLApp "\\8857") (Next (Var (Ident "l2")))))))))),Assign (Ident "take") (TSub "\\8592") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "n") (Abstr (Lam "\\955") (Ident "l") (Match (Var (Ident "l")) (Ident "end") (TMatch "\\8594") (Var (Ident "l")) (Ident "lst") (TMatch "\\8594") (Ite (Eq (Var (Ident "n")) (IVal 0)) (InL (Single (TSingle "\\120793"))) (App (App (Abstr (Lam "\\955") (Ident "e") (Abstr (Lam "\\955") (Ident "l") (InR (Pair (Var (Ident "e")) (Var (Ident "l")))))) (Fst (Var (Ident "lst")))) (PrevI (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Sub (Var (Ident "n")) (IVal 1)))) (TLApp "\\8857") (Next (Snd (Var (Ident "lst")))))))))))),Assign (Ident "drop") (TSub "\\8592") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "n") (Abstr (Lam "\\955") (Ident "l") (Match (Var (Ident "l")) (Ident "end") (TMatch "\\8594") (Var (Ident "l")) (Ident "lst") (TMatch "\\8594") (App (Ite (Eq (Var (Ident "n")) (IVal 0)) (App (Abstr (Lam "\\955") (Ident "e") (Abstr (Lam "\\955") (Ident "l") (InR (Pair (Var (Ident "e")) (Var (Ident "l")))))) (Fst (Var (Ident "lst")))) (Abstr (Lam "\\955") (Ident "x") (Var (Ident "x")))) (PrevI (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Ite (Eq (Var (Ident "n")) (IVal 0)) (IVal 0) (Sub (Var (Ident "n")) (IVal 1))))) (TLApp "\\8857") (Next (Snd (Var (Ident "lst"))))))))))),Assign (Ident "filter") (TSub "\\8592") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "pred") (Abstr (Lam "\\955") (Ident "l") (Match (Var (Ident "l")) (Ident "end") (TMatch "\\8594") (Var (Ident "l")) (Ident "lst") (TMatch "\\8594") (App (Ite (App (Var (Ident "pred")) (Fst (Var (Ident "lst")))) (App (Abstr (Lam "\\955") (Ident "e") (Abstr (Lam "\\955") (Ident "l") (InR (Pair (Var (Ident "e")) (Var (Ident "l")))))) (Fst (Var (Ident "lst")))) (Abstr (Lam "\\955") (Ident "x") (Var (Ident "x")))) (PrevI (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Var (Ident "pred")))) (TLApp "\\8857") (Next (Snd (Var (Ident "lst"))))))))))),Assign (Ident "map") (TSub "\\8592") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "func") (Abstr (Lam "\\955") (Ident "list") (Match (Var (Ident "list")) (Ident "end") (TMatch "\\8594") (Var (Ident "list")) (Ident "lst") (TMatch "\\8594") (InR (Pair (App (Var (Ident "func")) (Fst (Var (Ident "lst")))) (PrevI (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Var (Ident "func")))) (TLApp "\\8857") (Next (Snd (Var (Ident "lst")))))))))))),Assign (Ident "foldl") (TSub "\\8592") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "func") (Abstr (Lam "\\955") (Ident "val") (Abstr (Lam "\\955") (Ident "list") (Match (Var (Ident "list")) (Ident "end") (TMatch "\\8594") (Var (Ident "val")) (Ident "lst") (TMatch "\\8594") (PrevI (LApp (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Var (Ident "func")))) (TLApp "\\8857") (Next (App (App (Var (Ident "func")) (Fst (Var (Ident "lst")))) (Var (Ident "val"))))) (TLApp "\\8857") (Next (Snd (Var (Ident "lst"))))))))))),Assign (Ident "foldr") (TSub "\\8592") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "func") (Abstr (Lam "\\955") (Ident "val") (Abstr (Lam "\\955") (Ident "list") (Match (Var (Ident "list")) (Ident "end") (TMatch "\\8594") (Var (Ident "val")) (Ident "lst") (TMatch "\\8594") (App (App (Var (Ident "func")) (PrevI (LApp (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Var (Ident "func")))) (TLApp "\\8857") (Next (Var (Ident "val")))) (TLApp "\\8857") (Next (Snd (Var (Ident "lst"))))))) (Fst (Var (Ident "lst")))))))))]