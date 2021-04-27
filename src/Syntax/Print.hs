{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | Pretty-printer for Syntax.
--   Generated by the BNF converter.

module Syntax.Print where

import qualified Syntax.Abs
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

instance Print Syntax.Abs.Ident where
  prt _ (Syntax.Abs.Ident i) = doc $ showString $ i

instance Print Syntax.Abs.Lam where
  prt _ (Syntax.Abs.Lam i) = doc $ showString $ i

instance Print Syntax.Abs.Conj where
  prt _ (Syntax.Abs.Conj i) = doc $ showString $ i

instance Print Syntax.Abs.Disj where
  prt _ (Syntax.Abs.Disj i) = doc $ showString $ i

instance Print Syntax.Abs.TNot where
  prt _ (Syntax.Abs.TNot i) = doc $ showString $ i

instance Print Syntax.Abs.TLeq where
  prt _ (Syntax.Abs.TLeq i) = doc $ showString $ i

instance Print Syntax.Abs.TGeq where
  prt _ (Syntax.Abs.TGeq i) = doc $ showString $ i

instance Print Syntax.Abs.TLApp where
  prt _ (Syntax.Abs.TLApp i) = doc $ showString $ i

instance Print Syntax.Abs.TSub where
  prt _ (Syntax.Abs.TSub i) = doc $ showString $ i

instance Print Syntax.Abs.TMatch where
  prt _ (Syntax.Abs.TMatch i) = doc $ showString $ i

instance Print Syntax.Abs.BConst where
  prt i e = case e of
    Syntax.Abs.BTrue -> prPrec i 0 (concatD [doc (showString "true")])
    Syntax.Abs.BFalse -> prPrec i 0 (concatD [doc (showString "false")])

instance Print Syntax.Abs.Exp where
  prt i e = case e of
    Syntax.Abs.Var id -> prPrec i 11 (concatD [prt 0 id])
    Syntax.Abs.Val d -> prPrec i 11 (concatD [prt 0 d])
    Syntax.Abs.BVal bconst -> prPrec i 11 (concatD [prt 0 bconst])
    Syntax.Abs.Next exp -> prPrec i 10 (concatD [doc (showString "next"), prt 11 exp])
    Syntax.Abs.Prev environment exp -> prPrec i 10 (concatD [doc (showString "prev"), doc (showString "{"), prt 0 environment, doc (showString "}"), doc (showString "."), prt 11 exp])
    Syntax.Abs.PrevE exp -> prPrec i 10 (concatD [doc (showString "prev"), prt 11 exp])
    Syntax.Abs.PrevI exp -> prPrec i 10 (concatD [doc (showString "prevI"), prt 11 exp])
    Syntax.Abs.Box environment exp -> prPrec i 10 (concatD [doc (showString "box"), doc (showString "{"), prt 0 environment, doc (showString "}"), doc (showString "."), prt 11 exp])
    Syntax.Abs.BoxI exp -> prPrec i 10 (concatD [doc (showString "boxI"), prt 11 exp])
    Syntax.Abs.Unbox exp -> prPrec i 10 (concatD [doc (showString "unbox"), prt 11 exp])
    Syntax.Abs.In exp -> prPrec i 10 (concatD [doc (showString "in"), prt 11 exp])
    Syntax.Abs.Out exp -> prPrec i 10 (concatD [doc (showString "out"), prt 11 exp])
    Syntax.Abs.Fst exp -> prPrec i 10 (concatD [doc (showString "fst"), prt 11 exp])
    Syntax.Abs.Snd exp -> prPrec i 10 (concatD [doc (showString "snd"), prt 11 exp])
    Syntax.Abs.InL exp -> prPrec i 10 (concatD [doc (showString "inL"), prt 11 exp])
    Syntax.Abs.InR exp -> prPrec i 10 (concatD [doc (showString "inR"), prt 11 exp])
    Syntax.Abs.App exp1 exp2 -> prPrec i 9 (concatD [prt 9 exp1, prt 10 exp2])
    Syntax.Abs.LApp exp1 tlapp exp2 -> prPrec i 9 (concatD [prt 9 exp1, prt 0 tlapp, prt 10 exp2])
    Syntax.Abs.Mul exp1 exp2 -> prPrec i 8 (concatD [prt 8 exp1, doc (showString "*"), prt 9 exp2])
    Syntax.Abs.Div exp1 exp2 -> prPrec i 8 (concatD [prt 8 exp1, doc (showString "/"), prt 9 exp2])
    Syntax.Abs.Add exp1 exp2 -> prPrec i 7 (concatD [prt 7 exp1, doc (showString "+"), prt 8 exp2])
    Syntax.Abs.Sub exp1 exp2 -> prPrec i 7 (concatD [prt 7 exp1, doc (showString "-"), prt 8 exp2])
    Syntax.Abs.Eq exp1 exp2 -> prPrec i 6 (concatD [prt 6 exp1, doc (showString "="), prt 7 exp2])
    Syntax.Abs.Lt exp1 exp2 -> prPrec i 6 (concatD [prt 6 exp1, doc (showString "<"), prt 7 exp2])
    Syntax.Abs.Gt exp1 exp2 -> prPrec i 6 (concatD [prt 6 exp1, doc (showString ">"), prt 7 exp2])
    Syntax.Abs.Leq exp1 tleq exp2 -> prPrec i 6 (concatD [prt 6 exp1, prt 0 tleq, prt 7 exp2])
    Syntax.Abs.Geq exp1 tgeq exp2 -> prPrec i 6 (concatD [prt 6 exp1, prt 0 tgeq, prt 7 exp2])
    Syntax.Abs.Not tnot exp -> prPrec i 5 (concatD [prt 0 tnot, prt 6 exp])
    Syntax.Abs.And exp1 conj exp2 -> prPrec i 4 (concatD [prt 4 exp1, prt 0 conj, prt 5 exp2])
    Syntax.Abs.Or exp1 disj exp2 -> prPrec i 4 (concatD [prt 4 exp1, prt 0 disj, prt 5 exp2])
    Syntax.Abs.Pair exp1 exp2 -> prPrec i 3 (concatD [doc (showString "["), prt 3 exp1, doc (showString ","), prt 3 exp2, doc (showString "]")])
    Syntax.Abs.Norm exp -> prPrec i 3 (concatD [doc (showString "normal"), prt 3 exp])
    Syntax.Abs.Ite exp1 exp2 exp3 -> prPrec i 2 (concatD [doc (showString "if"), prt 4 exp1, doc (showString "then"), prt 7 exp2, doc (showString "else"), prt 7 exp3])
    Syntax.Abs.Match exp1 id1 tmatch1 exp2 id2 tmatch2 exp3 -> prPrec i 1 (concatD [doc (showString "match"), prt 11 exp1, doc (showString "{"), prt 0 id1, prt 0 tmatch1, prt 1 exp2, doc (showString ";"), prt 0 id2, prt 0 tmatch2, prt 1 exp3, doc (showString "}")])
    Syntax.Abs.Abstr lam id exp -> prPrec i 0 (concatD [prt 0 lam, prt 0 id, doc (showString "."), prt 0 exp])
    Syntax.Abs.Rec id exp -> prPrec i 0 (concatD [doc (showString "fix"), prt 0 id, doc (showString "."), prt 0 exp])

instance Print Syntax.Abs.Prg where
  prt i e = case e of
    Syntax.Abs.DefProg environment exp -> prPrec i 0 (concatD [doc (showString "let"), prt 0 environment, doc (showString "in:"), prt 0 exp])
    Syntax.Abs.Prog exp -> prPrec i 0 (concatD [prt 0 exp])

instance Print Syntax.Abs.Assignment where
  prt i e = case e of
    Syntax.Abs.Assign id tsub exp -> prPrec i 0 (concatD [prt 0 id, prt 0 tsub, prt 0 exp])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print Syntax.Abs.Environment where
  prt i e = case e of
    Syntax.Abs.Env assignments -> prPrec i 0 (concatD [prt 0 assignments])

instance Print [Syntax.Abs.Assignment] where
  prt = prtList

