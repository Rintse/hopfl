
module Semantics.Builtins where

import Syntax.Raw.Abs

builtins :: [Assignment]
builtins =
    [Assign (Ident "exp") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (Pow (DVal 2.71828) (Var (Ident "x")))),Assign (Ident "min") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (Abstr (Lam "\\955") (Ident "y") (Ite (Lt (Var (Ident "x")) (Var (Ident "y"))) (Var (Ident "x")) (Var (Ident "y"))))),Assign (Ident "max") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (Abstr (Lam "\\955") (Ident "y") (Ite (Gt (Var (Ident "x")) (Var (Ident "y"))) (Var (Ident "x")) (Var (Ident "y"))))),Assign (Ident "odd") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (Eq (Mod (Var (Ident "x")) (IVal 2)) (IVal 1))),Assign (Ident "even") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (Eq (Mod (Var (Ident "x")) (IVal 2)) (IVal 0))),Assign (Ident "sqrt") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (Pow (Var (Ident "x")) (DVal 0.5))),Assign (Ident "add") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (Abstr (Lam "\\955") (Ident "y") (Add (Var (Ident "x")) (Var (Ident "y"))))),Assign (Ident "subtract") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (Abstr (Lam "\\955") (Ident "y") (Sub (Var (Ident "x")) (Var (Ident "y"))))),Assign (Ident "divide") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (Abstr (Lam "\\955") (Ident "y") (Div (Var (Ident "x")) (Var (Ident "y"))))),Assign (Ident "multiply") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (Abstr (Lam "\\955") (Ident "y") (Mul (Var (Ident "x")) (Var (Ident "y"))))),Assign (Ident "power") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (Abstr (Lam "\\955") (Ident "y") (Pow (Var (Ident "x")) (Var (Ident "y"))))),Assign (Ident "mod") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (Abstr (Lam "\\955") (Ident "y") (Mod (Var (Ident "x")) (Var (Ident "y"))))),Assign (Ident "xor") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "p") (Abstr (Lam "\\955") (Ident "q") (And (Or (Var (Ident "p")) (Disj "\\8744") (Var (Ident "q"))) (Conj "\\8743") (Not (TNot "\\172") (And (Var (Ident "p")) (Conj "\\8743") (Var (Ident "q"))))))),Assign (Ident "implies") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "p") (Abstr (Lam "\\955") (Ident "q") (Or (Not (TNot "\\172") (Var (Ident "p"))) (Disj "\\8744") (Var (Ident "q"))))),Assign (Ident "not") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "p") (Not (TNot "\\172") (Var (Ident "p"))))] ++
        [Assign (Ident "flip2") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "f") (Abstr (Lam "\\955") (Ident "a1") (Abstr (Lam "\\955") (Ident "a2") (App (App (Var (Ident "f")) (Var (Ident "a2"))) (Var (Ident "a1")))))),Assign (Ident "curry") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "f") (Abstr (Lam "\\955") (Ident "pair") (App (App (Var (Ident "f")) (Fst (Var (Ident "pair")))) (Snd (Var (Ident "pair")))))),Assign (Ident "uncurry") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "f") (Abstr (Lam "\\955") (Ident "a1") (Abstr (Lam "\\955") (Ident "a2") (App (Var (Ident "f")) (Pair (Var (Ident "a1")) (Var (Ident "a2")))))))] ++
        [Assign (Ident "co_zero") (TSub "\\8592") (In (InL (Single (TSingle "\\120793")))),Assign (Ident "co_inf") (TSub "\\8592") (Rec (Ident "f") (In (InR (Var (Ident "f"))))),Assign (Ident "co_succ") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "n") (In (InR (Next (Var (Ident "n")))))),Assign (Ident "co_isZero") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "n") (Match (Out (Var (Ident "n"))) (Ident "x") (TMatch "\\8594") (BVal BTrue) (Ident "y") (TMatch "\\8594") (BVal BFalse))),Assign (Ident "co_add") (TSub "\\8592") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "n1") (Abstr (Lam "\\955") (Ident "n2") (Match (Out (Var (Ident "n1"))) (Ident "x") (TMatch "\\8594") (Var (Ident "n2")) (Ident "y") (TMatch "\\8594") (In (InR (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Var (Ident "y"))) (TLApp "\\8857") (Next (Var (Ident "n2")))))))))),Assign (Ident "int_to_co") (TSub "\\8592") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "n") (Ite (Eq (Var (Ident "n")) (IVal 0)) (In (InL (Single (TSingle "\\120793")))) (In (InR (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Sub (Var (Ident "n")) (IVal 1))))))))),Assign (Ident "co_to_int") (TSub "\\8592") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "c") (Match (Out (Var (Ident "c"))) (Ident "x") (TMatch "\\8594") (IVal 0) (Ident "y") (TMatch "\\8594") (Add (IVal 1) (PrevI (LApp (Var (Ident "f")) (TLApp "\\8857") (Var (Ident "y"))))))))] ++
        [Assign (Ident "rand_idx") (TSub "\\8592") (App (App (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "r") (Abstr (Lam "\\955") (Ident "n") (Abstr (Lam "\\955") (Ident "size") (Ite (Leq (Var (Ident "r")) (TLeq "\\8804") (Mul (Var (Ident "n")) (Div (IVal 1) (Var (Ident "size"))))) (Sub (Var (Ident "n")) (IVal 1)) (PrevI (LApp (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Var (Ident "r")))) (TLApp "\\8857") (Next (Add (Var (Ident "n")) (IVal 1)))) (TLApp "\\8857") (Next (Var (Ident "size")))))))))) Rand) (IVal 0)),Assign (Ident "rand_idx2") (TSub "\\8592") (App (App (Abstr (Lam "\\955") (Ident "r") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "r") (Abstr (Lam "\\955") (Ident "n") (Abstr (Lam "\\955") (Ident "size") (Ite (Leq (Var (Ident "r")) (TLeq "\\8804") (Mul (Var (Ident "n")) (Div (IVal 1) (Var (Ident "size"))))) (In (Sub (InL (Var (Ident "n"))) (IVal 1))) (In (InR (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Add (Var (Ident "n")) (IVal 1)))) (TLApp "\\8857") (Next (Var (Ident "size")))))))))))) Rand) (IVal 0)),Assign (Ident "idx4_from_rand") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "r") (Ite (Lt (Var (Ident "r")) (DVal 0.25)) (IVal 0) (Ite (Lt (Var (Ident "r")) (DVal 0.5)) (IVal 1) (Ite (Lt (Var (Ident "r")) (DVal 0.75)) (IVal 2) (IVal 3)))))] ++
        [Assign (Ident "s_cons_g") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "e") (Abstr (Lam "\\955") (Ident "s") (In (Pair (Var (Ident "e")) (Next (Var (Ident "s"))))))),Assign (Ident "s_cons_g_n") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "e") (Abstr (Lam "\\955") (Ident "s") (In (Pair (Var (Ident "e")) (Var (Ident "s")))))),Assign (Ident "s_head_g") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "s") (Fst (Out (Var (Ident "s"))))),Assign (Ident "s_tail_g") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "s") (Snd (Out (Var (Ident "s"))))),Assign (Ident "s_idx_g") (TSub "\\8592") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "n") (Abstr (Lam "\\955") (Ident "s") (Ite (Eq (Var (Ident "n")) (IVal 0)) (In (InL (App (Abstr (Lam "\\955") (Ident "s") (Fst (Out (Var (Ident "s"))))) (Var (Ident "s"))))) (In (InR (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Sub (Var (Ident "n")) (IVal 1)))) (TLApp "\\8857") (App (Abstr (Lam "\\955") (Ident "s") (Snd (Out (Var (Ident "s"))))) (Var (Ident "s")))))))))),Assign (Ident "s_2nd_g") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "s") (LApp (Next (Abstr (Lam "\\955") (Ident "s") (Fst (Out (Var (Ident "s")))))) (TLApp "\\8857") (App (Abstr (Lam "\\955") (Ident "s") (Snd (Out (Var (Ident "s"))))) (Var (Ident "s"))))),Assign (Ident "s_3rd_g") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "s") (LApp (Next (Abstr (Lam "\\955") (Ident "s") (LApp (Next (Abstr (Lam "\\955") (Ident "s") (Fst (Out (Var (Ident "s")))))) (TLApp "\\8857") (App (Abstr (Lam "\\955") (Ident "s") (Snd (Out (Var (Ident "s"))))) (Var (Ident "s")))))) (TLApp "\\8857") (App (Abstr (Lam "\\955") (Ident "s") (Snd (Out (Var (Ident "s"))))) (Var (Ident "s"))))),Assign (Ident "s_4th_g") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "s") (LApp (Next (Abstr (Lam "\\955") (Ident "s") (LApp (Next (Abstr (Lam "\\955") (Ident "s") (LApp (Next (Abstr (Lam "\\955") (Ident "s") (Fst (Out (Var (Ident "s")))))) (TLApp "\\8857") (App (Abstr (Lam "\\955") (Ident "s") (Snd (Out (Var (Ident "s"))))) (Var (Ident "s")))))) (TLApp "\\8857") (App (Abstr (Lam "\\955") (Ident "s") (Snd (Out (Var (Ident "s"))))) (Var (Ident "s")))))) (TLApp "\\8857") (App (Abstr (Lam "\\955") (Ident "s") (Snd (Out (Var (Ident "s"))))) (Var (Ident "s"))))),Assign (Ident "s_map_g") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "func") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "s") (App (App (Abstr (Lam "\\955") (Ident "e") (Abstr (Lam "\\955") (Ident "s") (In (Pair (Var (Ident "e")) (Var (Ident "s")))))) (App (Var (Ident "func")) (App (Abstr (Lam "\\955") (Ident "s") (Fst (Out (Var (Ident "s"))))) (Var (Ident "s"))))) (LApp (Var (Ident "f")) (TLApp "\\8857") (App (Abstr (Lam "\\955") (Ident "s") (Snd (Out (Var (Ident "s"))))) (Var (Ident "s")))))))),Assign (Ident "s_head_c") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "s1") (App (Abstr (Lam "\\955") (Ident "s") (Fst (Out (Var (Ident "s"))))) (Unbox (Var (Ident "s1"))))),Assign (Ident "s_tail_c") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "s2") (BoxI (PrevI (App (Abstr (Lam "\\955") (Ident "s") (Snd (Out (Var (Ident "s"))))) (Unbox (Var (Ident "s2"))))))),Assign (Ident "s_idx_c") (TSub "\\8592") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "n") (Abstr (Lam "\\955") (Ident "s") (Ite (Eq (Var (Ident "n")) (IVal 0)) (In (InL (App (Abstr (Lam "\\955") (Ident "s1") (App (Abstr (Lam "\\955") (Ident "s") (Fst (Out (Var (Ident "s"))))) (Unbox (Var (Ident "s1"))))) (Var (Ident "s"))))) (In (InR (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Sub (Var (Ident "n")) (IVal 1)))) (TLApp "\\8857") (Next (App (Abstr (Lam "\\955") (Ident "s2") (BoxI (PrevI (App (Abstr (Lam "\\955") (Ident "s") (Snd (Out (Var (Ident "s"))))) (Unbox (Var (Ident "s2"))))))) (Var (Ident "s"))))))))))),Assign (Ident "s_2nd_c") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "s") (App (Abstr (Lam "\\955") (Ident "s1") (App (Abstr (Lam "\\955") (Ident "s") (Fst (Out (Var (Ident "s"))))) (Unbox (Var (Ident "s1"))))) (App (Abstr (Lam "\\955") (Ident "s2") (BoxI (PrevI (App (Abstr (Lam "\\955") (Ident "s") (Snd (Out (Var (Ident "s"))))) (Unbox (Var (Ident "s2"))))))) (Var (Ident "s"))))),Assign (Ident "s_3rd_c") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "s") (App (Abstr (Lam "\\955") (Ident "s1") (App (Abstr (Lam "\\955") (Ident "s") (Fst (Out (Var (Ident "s"))))) (Unbox (Var (Ident "s1"))))) (App (Abstr (Lam "\\955") (Ident "s2") (BoxI (PrevI (App (Abstr (Lam "\\955") (Ident "s") (Snd (Out (Var (Ident "s"))))) (Unbox (Var (Ident "s2"))))))) (App (Abstr (Lam "\\955") (Ident "s2") (BoxI (PrevI (App (Abstr (Lam "\\955") (Ident "s") (Snd (Out (Var (Ident "s"))))) (Unbox (Var (Ident "s2"))))))) (Var (Ident "s")))))),Assign (Ident "s_4th_c") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "s") (App (Abstr (Lam "\\955") (Ident "s1") (App (Abstr (Lam "\\955") (Ident "s") (Fst (Out (Var (Ident "s"))))) (Unbox (Var (Ident "s1"))))) (App (Abstr (Lam "\\955") (Ident "s2") (BoxI (PrevI (App (Abstr (Lam "\\955") (Ident "s") (Snd (Out (Var (Ident "s"))))) (Unbox (Var (Ident "s2"))))))) (App (Abstr (Lam "\\955") (Ident "s2") (BoxI (PrevI (App (Abstr (Lam "\\955") (Ident "s") (Snd (Out (Var (Ident "s"))))) (Unbox (Var (Ident "s2"))))))) (App (Abstr (Lam "\\955") (Ident "s2") (BoxI (PrevI (App (Abstr (Lam "\\955") (Ident "s") (Snd (Out (Var (Ident "s"))))) (Unbox (Var (Ident "s2"))))))) (Var (Ident "s"))))))),Assign (Ident "s_limit_c") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "func") (Abstr (Lam "\\955") (Ident "x") (BoxI (App (Unbox (Var (Ident "func"))) (Unbox (Var (Ident "x"))))))),Assign (Ident "s_map_c") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "f") (App (Abstr (Lam "\\955") (Ident "func") (Abstr (Lam "\\955") (Ident "x") (BoxI (App (Unbox (Var (Ident "func"))) (Unbox (Var (Ident "x"))))))) (BoxI (App (Abstr (Lam "\\955") (Ident "func") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "s") (App (App (Abstr (Lam "\\955") (Ident "e") (Abstr (Lam "\\955") (Ident "s") (In (Pair (Var (Ident "e")) (Var (Ident "s")))))) (App (Var (Ident "func")) (App (Abstr (Lam "\\955") (Ident "s") (Fst (Out (Var (Ident "s"))))) (Var (Ident "s"))))) (LApp (Var (Ident "f")) (TLApp "\\8857") (App (Abstr (Lam "\\955") (Ident "s") (Snd (Out (Var (Ident "s"))))) (Var (Ident "s")))))))) (Var (Ident "f"))))))] ++
        [Assign (Ident "dres_now") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (In (InL (Var (Ident "x"))))),Assign (Ident "dres_delay") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (In (InR (Next (Var (Ident "x")))))),Assign (Ident "dres_never") (TSub "\\8592") (Rec (Ident "f") (In (InR (Var (Ident "f"))))),Assign (Ident "dres_later") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "x") (In (InR (Var (Ident "x"))))),Assign (Ident "dres_map") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "func") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "x") (Match (Out (Var (Ident "x"))) (Ident "value") (TMatch "\\8594") (App (Abstr (Lam "\\955") (Ident "x") (In (InL (Var (Ident "x"))))) (App (Var (Ident "func")) (Var (Ident "value")))) (Ident "delay") (TMatch "\\8594") (App (Abstr (Lam "\\955") (Ident "x") (In (InR (Var (Ident "x"))))) (LApp (Var (Ident "f")) (TLApp "\\8857") (Var (Ident "delay")))))))),Assign (Ident "dres_func2_1") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "func") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "a") (Abstr (Lam "\\955") (Ident "x") (Match (Out (Var (Ident "x"))) (Ident "value") (TMatch "\\8594") (App (Abstr (Lam "\\955") (Ident "x") (In (InL (Var (Ident "x"))))) (App (App (Var (Ident "func")) (Var (Ident "a"))) (Var (Ident "value")))) (Ident "delay") (TMatch "\\8594") (App (Abstr (Lam "\\955") (Ident "x") (In (InR (Var (Ident "x"))))) (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Var (Ident "a")))) (TLApp "\\8857") (Var (Ident "delay"))))))))),Assign (Ident "flip2") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "f") (Abstr (Lam "\\955") (Ident "a1") (Abstr (Lam "\\955") (Ident "a2") (App (App (Var (Ident "f")) (Var (Ident "a2"))) (Var (Ident "a1")))))),Assign (Ident "dres_func2") (TSub "\\8592") (Abstr (Lam "\\955") (Ident "func") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "x1") (Abstr (Lam "\\955") (Ident "x2") (Match (Out (Var (Ident "x1"))) (Ident "value1") (TMatch "\\8594") (App (App (App (Abstr (Lam "\\955") (Ident "func") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "a") (Abstr (Lam "\\955") (Ident "x") (Match (Out (Var (Ident "x"))) (Ident "value") (TMatch "\\8594") (App (Abstr (Lam "\\955") (Ident "x") (In (InL (Var (Ident "x"))))) (App (App (Var (Ident "func")) (Var (Ident "a"))) (Var (Ident "value")))) (Ident "delay") (TMatch "\\8594") (App (Abstr (Lam "\\955") (Ident "x") (In (InR (Var (Ident "x"))))) (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Var (Ident "a")))) (TLApp "\\8857") (Var (Ident "delay"))))))))) (Var (Ident "func"))) (Var (Ident "value1"))) (Var (Ident "x2"))) (Ident "delay1") (TMatch "\\8594") (Match (Out (Var (Ident "x2"))) (Ident "value2") (TMatch "\\8594") (App (App (App (Abstr (Lam "\\955") (Ident "func") (Rec (Ident "f") (Abstr (Lam "\\955") (Ident "a") (Abstr (Lam "\\955") (Ident "x") (Match (Out (Var (Ident "x"))) (Ident "value") (TMatch "\\8594") (App (Abstr (Lam "\\955") (Ident "x") (In (InL (Var (Ident "x"))))) (App (App (Var (Ident "func")) (Var (Ident "a"))) (Var (Ident "value")))) (Ident "delay") (TMatch "\\8594") (App (Abstr (Lam "\\955") (Ident "x") (In (InR (Var (Ident "x"))))) (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Next (Var (Ident "a")))) (TLApp "\\8857") (Var (Ident "delay"))))))))) (App (Abstr (Lam "\\955") (Ident "f") (Abstr (Lam "\\955") (Ident "a1") (Abstr (Lam "\\955") (Ident "a2") (App (App (Var (Ident "f")) (Var (Ident "a2"))) (Var (Ident "a1")))))) (Var (Ident "func")))) (Var (Ident "value2"))) (Var (Ident "x1"))) (Ident "delay2") (TMatch "\\8594") (App (Abstr (Lam "\\955") (Ident "x") (In (InR (Var (Ident "x"))))) (LApp (LApp (Var (Ident "f")) (TLApp "\\8857") (Var (Ident "delay1"))) (TLApp "\\8857") (Var (Ident "delay2"))))))))))]