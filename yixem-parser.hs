{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Yixem.Parser where

import Language.LBNF

bnfc [lbnf|

antiquote "[" ":" ":]";

LV.   Vars ::= Ident ;
EV.   Vars ::= Ident "," Vars ;

SVar.  Stm ::= "let" Ident "=" Expr;
SFun.  Stm ::= "fun" Ident  "(" Vars ")" "=" Expr;

[].       [Stm]    ::= ;
(:).      [Stm]    ::= Stm [Stm];

Prog. Program ::= "module" Ident "where" [Stm];


OEq.    Operator ::= "==";
ONeq.   Operator ::= "/=";
OPlus.  Operator ::= "+";
OMinus. Operator ::= "-";
OMult.  Operator ::= "*";
OGt.    Operator ::= ">";
OLt.    Operator ::= "<";
OAnd.   Operator ::= "&&";
OOr.    Operator ::= "||";
OXor.   Operator ::= "^";

VTrue.  VBool ::= "#t";
VFalse. VBool ::= "#f";

LE.   ExLst ::= Expr ;
EE.   ExLst ::= Expr "," ExLst ;



ECall.  Expr1 ::= Ident "[" ExLst "]";
ELet.   Expr1 ::= "let" Ident "=" Expr "in" Expr;
EIf.    Expr2 ::= "if" Expr2 "then" Expr0 "else" Expr0;
EWhen.  Expr2 ::= "when" Expr2 "then" Expr0;
EOp.    Expr3 ::= Expr3 Operator Expr1;
EBool.  Expr4 ::= VBool;
EUnit.  Expr4 ::= "()";
ELit.   Expr4 ::= Integer;
EVar.   Expr4 ::= Ident;
_.      Expr  ::= Expr0;
_.      Expr0 ::= Expr1;
_.      Expr1 ::= Expr2;
_.      Expr2 ::= Expr3;
_.      Expr3 ::= Expr4;
_.      Expr4 ::= "(" Expr ")";

-- pragmas

comment "{-" "-}" ;
comment "--" ;

entrypoints Program ;
|]

parserX :: String -> Either String Program
parserX str =
  case pProgram (tokens str) of
	Ok x  -> Right x
	Bad x -> Left x