{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | Pretty-printer for Syntax.
--   Generated by the BNF converter.

module Syntax.Raw.Print where

import qualified Syntax.Raw.Abs
import Data.Char

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    [";"]        -> showChar ';'
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : ts@(p:_) | closingOrPunctuation p -> showString t . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i     = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print Syntax.Raw.Abs.Ident where
  prt _ (Syntax.Raw.Abs.Ident i) = doc $ showString $ i

instance Print Syntax.Raw.Abs.Lam where
  prt _ (Syntax.Raw.Abs.Lam i) = doc $ showString $ i

instance Print Syntax.Raw.Abs.Conj where
  prt _ (Syntax.Raw.Abs.Conj i) = doc $ showString $ i

instance Print Syntax.Raw.Abs.Disj where
  prt _ (Syntax.Raw.Abs.Disj i) = doc $ showString $ i

instance Print Syntax.Raw.Abs.TNot where
  prt _ (Syntax.Raw.Abs.TNot i) = doc $ showString $ i

instance Print Syntax.Raw.Abs.TLeq where
  prt _ (Syntax.Raw.Abs.TLeq i) = doc $ showString $ i

instance Print Syntax.Raw.Abs.TGeq where
  prt _ (Syntax.Raw.Abs.TGeq i) = doc $ showString $ i

instance Print Syntax.Raw.Abs.TLApp where
  prt _ (Syntax.Raw.Abs.TLApp i) = doc $ showString $ i

instance Print Syntax.Raw.Abs.TSub where
  prt _ (Syntax.Raw.Abs.TSub i) = doc $ showString $ i

instance Print Syntax.Raw.Abs.TMatch where
  prt _ (Syntax.Raw.Abs.TMatch i) = doc $ showString $ i

instance Print Syntax.Raw.Abs.TSingle where
  prt _ (Syntax.Raw.Abs.TSingle i) = doc $ showString $ i

instance Print Syntax.Raw.Abs.BConst where
  prt i e = case e of
    Syntax.Raw.Abs.BTrue -> prPrec i 0 (concatD [doc (showString "true")])
    Syntax.Raw.Abs.BFalse -> prPrec i 0 (concatD [doc (showString "false")])

instance Print Syntax.Raw.Abs.Exp where
  prt i e = case e of
    Syntax.Raw.Abs.Single tsingle -> prPrec i 13 (concatD [prt 0 tsingle])
    Syntax.Raw.Abs.Var id -> prPrec i 13 (concatD [prt 0 id])
    Syntax.Raw.Abs.DVal d -> prPrec i 13 (concatD [prt 0 d])
    Syntax.Raw.Abs.IVal n -> prPrec i 13 (concatD [prt 0 n])
    Syntax.Raw.Abs.BVal bconst -> prPrec i 13 (concatD [prt 0 bconst])
    Syntax.Raw.Abs.EList lst -> prPrec i 13 (concatD [prt 0 lst])
    Syntax.Raw.Abs.Pair exp1 exp2 -> prPrec i 13 (concatD [doc (showString "("), prt 0 exp1, doc (showString ","), prt 1 exp2, doc (showString ")")])
    Syntax.Raw.Abs.Next exp -> prPrec i 12 (concatD [doc (showString "next"), prt 13 exp])
    Syntax.Raw.Abs.Prev environment exp -> prPrec i 12 (concatD [doc (showString "prev"), doc (showString "{"), prt 0 environment, doc (showString "}"), doc (showString "."), prt 13 exp])
    Syntax.Raw.Abs.PrevE exp -> prPrec i 12 (concatD [doc (showString "prev"), prt 13 exp])
    Syntax.Raw.Abs.PrevI exp -> prPrec i 12 (concatD [doc (showString "prevI"), prt 13 exp])
    Syntax.Raw.Abs.Box environment exp -> prPrec i 12 (concatD [doc (showString "box"), doc (showString "{"), prt 0 environment, doc (showString "}"), doc (showString "."), prt 12 exp])
    Syntax.Raw.Abs.BoxI exp -> prPrec i 12 (concatD [doc (showString "boxI"), prt 13 exp])
    Syntax.Raw.Abs.Unbox exp -> prPrec i 12 (concatD [doc (showString "unbox"), prt 13 exp])
    Syntax.Raw.Abs.Force exp -> prPrec i 12 (concatD [doc (showString "force"), prt 12 exp])
    Syntax.Raw.Abs.Rand -> prPrec i 12 (concatD [doc (showString "rand")])
    Syntax.Raw.Abs.ListIndex exp1 exp2 -> prPrec i 11 (concatD [prt 11 exp1, doc (showString "|"), prt 12 exp2, doc (showString "|")])
    Syntax.Raw.Abs.In exp -> prPrec i 11 (concatD [doc (showString "in"), prt 12 exp])
    Syntax.Raw.Abs.Out exp -> prPrec i 11 (concatD [doc (showString "out"), prt 12 exp])
    Syntax.Raw.Abs.Fst exp -> prPrec i 11 (concatD [doc (showString "fst"), prt 12 exp])
    Syntax.Raw.Abs.Snd exp -> prPrec i 11 (concatD [doc (showString "snd"), prt 12 exp])
    Syntax.Raw.Abs.InL exp -> prPrec i 11 (concatD [doc (showString "inL"), prt 12 exp])
    Syntax.Raw.Abs.InR exp -> prPrec i 11 (concatD [doc (showString "inR"), prt 12 exp])
    Syntax.Raw.Abs.ListHead exp -> prPrec i 11 (concatD [doc (showString "head"), prt 12 exp])
    Syntax.Raw.Abs.ListTail exp -> prPrec i 11 (concatD [doc (showString "tail"), prt 12 exp])
    Syntax.Raw.Abs.ListNull exp -> prPrec i 11 (concatD [doc (showString "null"), prt 12 exp])
    Syntax.Raw.Abs.ListLength exp -> prPrec i 11 (concatD [doc (showString "length"), prt 12 exp])
    Syntax.Raw.Abs.ListFold exp1 exp2 exp3 -> prPrec i 11 (concatD [doc (showString "foldl"), prt 12 exp1, prt 12 exp2, prt 12 exp3])
    Syntax.Raw.Abs.ListMap exp1 exp2 -> prPrec i 11 (concatD [doc (showString "map"), prt 12 exp1, prt 12 exp2])
    Syntax.Raw.Abs.ListElem exp1 exp2 -> prPrec i 11 (concatD [doc (showString "elem"), prt 12 exp1, prt 12 exp2])
    Syntax.Raw.Abs.ListTake exp1 exp2 -> prPrec i 11 (concatD [doc (showString "take"), prt 12 exp1, prt 12 exp2])
    Syntax.Raw.Abs.ListDrop exp1 exp2 -> prPrec i 11 (concatD [doc (showString "drop"), prt 12 exp1, prt 12 exp2])
    Syntax.Raw.Abs.App exp1 exp2 -> prPrec i 10 (concatD [prt 10 exp1, prt 11 exp2])
    Syntax.Raw.Abs.LApp exp1 tlapp exp2 -> prPrec i 10 (concatD [prt 10 exp1, prt 0 tlapp, prt 11 exp2])
    Syntax.Raw.Abs.ListCons exp1 exp2 -> prPrec i 10 (concatD [prt 10 exp1, doc (showString ":"), prt 11 exp2])
    Syntax.Raw.Abs.ListAppend exp1 exp2 -> prPrec i 10 (concatD [prt 10 exp1, doc (showString "++"), prt 11 exp2])
    Syntax.Raw.Abs.Min exp -> prPrec i 9 (concatD [doc (showString "-"), prt 10 exp])
    Syntax.Raw.Abs.Pow exp1 exp2 -> prPrec i 9 (concatD [prt 9 exp1, doc (showString "^"), prt 10 exp2])
    Syntax.Raw.Abs.Mul exp1 exp2 -> prPrec i 8 (concatD [prt 8 exp1, doc (showString "*"), prt 9 exp2])
    Syntax.Raw.Abs.Div exp1 exp2 -> prPrec i 8 (concatD [prt 8 exp1, doc (showString "/"), prt 9 exp2])
    Syntax.Raw.Abs.Mod exp1 exp2 -> prPrec i 8 (concatD [prt 8 exp1, doc (showString "%"), prt 9 exp2])
    Syntax.Raw.Abs.Add exp1 exp2 -> prPrec i 7 (concatD [prt 7 exp1, doc (showString "+"), prt 8 exp2])
    Syntax.Raw.Abs.Sub exp1 exp2 -> prPrec i 7 (concatD [prt 7 exp1, doc (showString "-"), prt 8 exp2])
    Syntax.Raw.Abs.Eq exp1 exp2 -> prPrec i 6 (concatD [prt 6 exp1, doc (showString "="), prt 7 exp2])
    Syntax.Raw.Abs.Lt exp1 exp2 -> prPrec i 6 (concatD [prt 6 exp1, doc (showString "<"), prt 7 exp2])
    Syntax.Raw.Abs.Gt exp1 exp2 -> prPrec i 6 (concatD [prt 6 exp1, doc (showString ">"), prt 7 exp2])
    Syntax.Raw.Abs.Leq exp1 tleq exp2 -> prPrec i 6 (concatD [prt 6 exp1, prt 0 tleq, prt 7 exp2])
    Syntax.Raw.Abs.Geq exp1 tgeq exp2 -> prPrec i 6 (concatD [prt 6 exp1, prt 0 tgeq, prt 7 exp2])
    Syntax.Raw.Abs.Not tnot exp -> prPrec i 5 (concatD [prt 0 tnot, prt 6 exp])
    Syntax.Raw.Abs.And exp1 conj exp2 -> prPrec i 4 (concatD [prt 4 exp1, prt 0 conj, prt 5 exp2])
    Syntax.Raw.Abs.Or exp1 disj exp2 -> prPrec i 4 (concatD [prt 4 exp1, prt 0 disj, prt 5 exp2])
    Syntax.Raw.Abs.Norm exp -> prPrec i 3 (concatD [doc (showString "normal"), prt 3 exp])
    Syntax.Raw.Abs.Ite exp1 exp2 exp3 -> prPrec i 2 (concatD [doc (showString "if"), prt 13 exp1, doc (showString "then"), prt 3 exp2, doc (showString "else"), prt 3 exp3])
    Syntax.Raw.Abs.Match exp1 id1 tmatch1 exp2 id2 tmatch2 exp3 -> prPrec i 1 (concatD [doc (showString "match"), prt 13 exp1, doc (showString "{"), doc (showString "inL"), prt 0 id1, prt 0 tmatch1, prt 2 exp2, doc (showString ";"), doc (showString "inR"), prt 0 id2, prt 0 tmatch2, prt 2 exp3, doc (showString "}")])
    Syntax.Raw.Abs.Abstr lam id exp -> prPrec i 0 (concatD [prt 0 lam, prt 0 id, doc (showString "."), prt 0 exp])
    Syntax.Raw.Abs.Rec id exp -> prPrec i 0 (concatD [doc (showString "fix"), prt 0 id, doc (showString "."), prt 0 exp])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print Syntax.Raw.Abs.Lst where
  prt i e = case e of
    Syntax.Raw.Abs.List exps -> prPrec i 0 (concatD [doc (showString "["), prt 0 exps, doc (showString "]")])

instance Print [Syntax.Raw.Abs.Exp] where
  prt = prtList

instance Print Syntax.Raw.Abs.Prg where
  prt i e = case e of
    Syntax.Raw.Abs.DefProg environment exp -> prPrec i 0 (concatD [doc (showString "let"), prt 0 environment, doc (showString "in:"), prt 0 exp])
    Syntax.Raw.Abs.Prog exp -> prPrec i 0 (concatD [prt 0 exp])

instance Print Syntax.Raw.Abs.Environment where
  prt i e = case e of
    Syntax.Raw.Abs.Env assignments -> prPrec i 0 (concatD [prt 0 assignments])

instance Print Syntax.Raw.Abs.Assignment where
  prt i e = case e of
    Syntax.Raw.Abs.Assign id tsub exp -> prPrec i 0 (concatD [prt 0 id, prt 0 tsub, prt 0 exp])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print [Syntax.Raw.Abs.Assignment] where
  prt = prtList

