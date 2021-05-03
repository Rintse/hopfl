
module Semantics.Builtins where

import Syntax.Raw.Abs

builtins :: [Assignment]
builtins = [
    Assign (Ident "tail_g") (TSub "") $
        Abstr (Lam "\\955") (Ident "s") (BoxI (PrevI (Snd (Out (Unbox (Var (Ident "s"))))))),
    Assign (Ident "head_g") (TSub "") $
        Abstr (Lam "\\955") (Ident "s") (Fst (Out (Unbox (Var (Ident "s"))))),
    Assign (Ident "map") (TSub "") $
        Rec (Ident "f") (Abstr (Lam "\\955") (Ident "func") (Abstr (Lam "\\955") (Ident "list") (Match (Var (Ident "list")) (Ident "end") (TMatch "\\8594") (Var (Ident "list")) (Ident "lst") (TMatch "\\8594") (InR (Pair (App (Var (Ident "func")) (Fst (Var (Ident "lst")))) (PrevI (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Var (Ident "func")))) (TLApp "\\8857") (Next (Snd (Var (Ident "lst"))))))))))),
    Assign (Ident "foldr") (TSub "") $
        Rec (Ident "f") (Abstr (Lam "\\955") (Ident "func") (Abstr (Lam "\\955") (Ident "val") (Abstr (Lam "\\955") (Ident "list") (Match (Var (Ident "list")) (Ident "end") (TMatch "\\8594") (Var (Ident "val")) (Ident "lst") (TMatch "\\8594") (App (App (Var (Ident "func")) (PrevI (LApp (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Var (Ident "func")))) (TLApp "\\8857") (Next (Var (Ident "val")))) (TLApp "\\8857") (Next (Snd (Var (Ident "lst"))))))) (Fst (Var (Ident "lst")))))))),
    Assign (Ident "tail") (TSub "") $
        Abstr (Lam "\\955") (Ident "l") (Match (Var (Ident "l")) (Ident "x") (TMatch "\\8594") (Var (Ident "x")) (Ident "y") (TMatch "\\8594") (Snd (Var (Ident "y")))),
    Assign (Ident "head") (TSub "") $
        Abstr (Lam "\\955") (Ident "l") (Match (Var (Ident "l")) (Ident "x") (TMatch "\\8594") (BVal BFalse) (Ident "y") (TMatch "\\8594") (Fst (Var (Ident "y")))),
    Assign (Ident "length") (TSub "") $
        Rec (Ident "f") (Abstr (Lam "\\955") (Ident "l") (Match (Var (Ident "l")) (Ident "x") (TMatch "\\8594") (IVal 0) (Ident "y") (TMatch "\\8594") (Add (IVal 1) (PrevI (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Snd (Var (Ident "y"))))))))),
    Assign (Ident "foldl") (TSub "") $
        Rec (Ident "f") (Abstr (Lam "\\955") (Ident "func") (Abstr (Lam "\\955") (Ident "val") (Abstr (Lam "\\955") (Ident "list") (Match (Var (Ident "list")) (Ident "end") (TMatch "\\8594") (Var (Ident "val")) (Ident "lst") (TMatch "\\8594") (PrevI (LApp (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Var (Ident "func")))) (TLApp "\\8857") (Next (App (App (Var (Ident "func")) (Fst (Var (Ident "lst")))) (Var (Ident "val"))))) (TLApp "\\8857") (Next (Snd (Var (Ident "lst"))))))))))
    ]