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

instance Print Syntax.Abs.Mu where
  prt _ (Syntax.Abs.Mu i) = doc $ showString $ i

instance Print Syntax.Abs.Prod where
  prt _ (Syntax.Abs.Prod i) = doc $ showString $ i

instance Print Syntax.Abs.To where
  prt _ (Syntax.Abs.To i) = doc $ showString $ i

instance Print Syntax.Abs.Later where
  prt _ (Syntax.Abs.Later i) = doc $ showString $ i

instance Print Syntax.Abs.Lapp where
  prt _ (Syntax.Abs.Lapp i) = doc $ showString $ i

instance Print Syntax.Abs.Typ where
  prt i e = case e of
    Syntax.Abs.TReal -> prPrec i 4 (concatD [doc (showString "real")])
    Syntax.Abs.TVar id -> prPrec i 4 (concatD [prt 0 id])
    Syntax.Abs.TLat later typ -> prPrec i 3 (concatD [prt 0 later, prt 3 typ])
    Syntax.Abs.TPRod typ1 prod typ2 -> prPrec i 2 (concatD [prt 1 typ1, prt 0 prod, prt 2 typ2])
    Syntax.Abs.TRec mu id typ -> prPrec i 1 (concatD [prt 0 mu, prt 0 id, doc (showString "."), prt 1 typ])
    Syntax.Abs.TFun typ1 to typ2 -> prPrec i 0 (concatD [prt 1 typ1, prt 0 to, prt 0 typ2])

instance Print Syntax.Abs.Exp where
  prt i e = case e of
    Syntax.Abs.Var id -> prPrec i 6 (concatD [prt 0 id])
    Syntax.Abs.Val d -> prPrec i 6 (concatD [prt 0 d])
    Syntax.Abs.Next exp -> prPrec i 5 (concatD [doc (showString "next"), prt 6 exp])
    Syntax.Abs.In exp -> prPrec i 5 (concatD [doc (showString "in"), prt 6 exp])
    Syntax.Abs.Out exp -> prPrec i 5 (concatD [doc (showString "out"), prt 6 exp])
    Syntax.Abs.App exp1 exp2 -> prPrec i 4 (concatD [prt 4 exp1, prt 4 exp2])
    Syntax.Abs.LApp exp1 lapp exp2 -> prPrec i 4 (concatD [prt 4 exp1, prt 0 lapp, prt 4 exp2])
    Syntax.Abs.Pair exp1 exp2 -> prPrec i 4 (concatD [doc (showString "<"), prt 4 exp1, doc (showString ","), prt 4 exp2, doc (showString ">")])
    Syntax.Abs.Fst exp -> prPrec i 4 (concatD [doc (showString "fst"), prt 4 exp])
    Syntax.Abs.Snd exp -> prPrec i 4 (concatD [doc (showString "snd"), prt 4 exp])
    Syntax.Abs.Norm exp -> prPrec i 4 (concatD [doc (showString "normal"), prt 4 exp])
    Syntax.Abs.Abstr lam exp1 exp2 -> prPrec i 1 (concatD [prt 0 lam, prt 6 exp1, doc (showString "."), prt 1 exp2])
    Syntax.Abs.Rec exp1 exp2 -> prPrec i 1 (concatD [doc (showString "fix"), prt 6 exp1, doc (showString "."), prt 1 exp2])
    Syntax.Abs.Typed exp typ -> prPrec i 0 (concatD [prt 0 exp, doc (showString "::"), prt 0 typ])

instance Print Syntax.Abs.Assignment where
  prt i e = case e of
    Syntax.Abs.Assign id exp -> prPrec i 0 (concatD [prt 0 id, doc (showString "="), prt 0 exp])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print Syntax.Abs.Environment where
  prt i e = case e of
    Syntax.Abs.Env assignments -> prPrec i 0 (concatD [prt 0 assignments])

instance Print [Syntax.Abs.Assignment] where
  prt = prtList
