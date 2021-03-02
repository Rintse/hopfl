-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Syntax.Par where
import qualified Syntax.Abs
import Syntax.Lex
}

%name pTyp Typ
%name pExp Exp
%name pEnvironment Environment
-- no lexer declaration
%monad { Either String } { (>>=) } { return }
%tokentype {Token}
%token
  '(' { PT _ (TS _ 1) }
  ')' { PT _ (TS _ 2) }
  ',' { PT _ (TS _ 3) }
  '.' { PT _ (TS _ 4) }
  '::' { PT _ (TS _ 5) }
  ';' { PT _ (TS _ 6) }
  '<' { PT _ (TS _ 7) }
  '=' { PT _ (TS _ 8) }
  '>' { PT _ (TS _ 9) }
  'fix' { PT _ (TS _ 10) }
  'fst' { PT _ (TS _ 11) }
  'in' { PT _ (TS _ 12) }
  'next' { PT _ (TS _ 13) }
  'normal' { PT _ (TS _ 14) }
  'out' { PT _ (TS _ 15) }
  'real' { PT _ (TS _ 16) }
  'snd' { PT _ (TS _ 17) }
  L_Ident  { PT _ (TV $$) }
  L_doubl  { PT _ (TD $$) }
  L_Lam { PT _ (T_Lam $$) }
  L_Mu { PT _ (T_Mu $$) }
  L_Prod { PT _ (T_Prod $$) }
  L_To { PT _ (T_To $$) }
  L_Later { PT _ (T_Later $$) }
  L_Lapp { PT _ (T_Lapp $$) }

%%

Ident :: { Syntax.Abs.Ident}
Ident  : L_Ident { Syntax.Abs.Ident $1 }

Double  :: { Double }
Double   : L_doubl  { (read ($1)) :: Double }

Lam :: { Syntax.Abs.Lam}
Lam  : L_Lam { Syntax.Abs.Lam $1 }

Mu :: { Syntax.Abs.Mu}
Mu  : L_Mu { Syntax.Abs.Mu $1 }

Prod :: { Syntax.Abs.Prod}
Prod  : L_Prod { Syntax.Abs.Prod $1 }

To :: { Syntax.Abs.To}
To  : L_To { Syntax.Abs.To $1 }

Later :: { Syntax.Abs.Later}
Later  : L_Later { Syntax.Abs.Later $1 }

Lapp :: { Syntax.Abs.Lapp}
Lapp  : L_Lapp { Syntax.Abs.Lapp $1 }

Typ4 :: { Syntax.Abs.Typ }
Typ4 : 'real' { Syntax.Abs.TReal }
     | Ident { Syntax.Abs.TVar $1 }
     | '(' Typ ')' { $2 }

Typ3 :: { Syntax.Abs.Typ }
Typ3 : Later Typ3 { Syntax.Abs.TLat $1 $2 } | Typ4 { $1 }

Typ2 :: { Syntax.Abs.Typ }
Typ2 : Typ1 Prod Typ2 { Syntax.Abs.TPRod $1 $2 $3 } | Typ3 { $1 }

Typ1 :: { Syntax.Abs.Typ }
Typ1 : Mu Ident '.' Typ1 { Syntax.Abs.TRec $1 $2 $4 } | Typ2 { $1 }

Typ :: { Syntax.Abs.Typ }
Typ : Typ1 To Typ { Syntax.Abs.TFun $1 $2 $3 } | Typ1 { $1 }

Exp6 :: { Syntax.Abs.Exp }
Exp6 : Ident { Syntax.Abs.Var $1 }
     | Double { Syntax.Abs.Val $1 }
     | '(' Exp ')' { $2 }

Exp5 :: { Syntax.Abs.Exp }
Exp5 : 'next' Exp6 { Syntax.Abs.Next $2 }
     | 'in' Exp6 { Syntax.Abs.In $2 }
     | 'out' Exp6 { Syntax.Abs.Out $2 }
     | Exp6 { $1 }

Exp4 :: { Syntax.Abs.Exp }
Exp4 : Exp4 Exp4 { Syntax.Abs.App $1 $2 }
     | Exp4 Lapp Exp4 { Syntax.Abs.LApp $1 $2 $3 }
     | '<' Exp4 ',' Exp4 '>' { Syntax.Abs.Pair $2 $4 }
     | 'fst' Exp4 { Syntax.Abs.Fst $2 }
     | 'snd' Exp4 { Syntax.Abs.Snd $2 }
     | 'normal' Exp4 { Syntax.Abs.Norm $2 }
     | Exp5 { $1 }

Exp1 :: { Syntax.Abs.Exp }
Exp1 : Lam Exp6 '.' Exp1 { Syntax.Abs.Abstr $1 $2 $4 }
     | 'fix' Exp6 '.' Exp1 { Syntax.Abs.Rec $2 $4 }
     | Exp2 { $1 }

Exp :: { Syntax.Abs.Exp }
Exp : Exp '::' Typ { Syntax.Abs.Typed $1 $3 } | Exp1 { $1 }

Exp2 :: { Syntax.Abs.Exp }
Exp2 : Exp3 { $1 }

Exp3 :: { Syntax.Abs.Exp }
Exp3 : Exp4 { $1 }

Assignment :: { Syntax.Abs.Assignment }
Assignment : Ident '=' Exp { Syntax.Abs.Assign $1 $3 }

Environment :: { Syntax.Abs.Environment }
Environment : ListAssignment { Syntax.Abs.Env $1 }

ListAssignment :: { [Syntax.Abs.Assignment] }
ListAssignment : {- empty -} { [] }
               | Assignment { (:[]) $1 }
               | Assignment ';' ListAssignment { (:) $1 $3 }
{

happyError :: [Token] -> Either String a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer = tokens
}

