module Yixem.AST (
  Module(..),
  Definition(..),
  Expr(..),
  Operator(..),
  ELit(..)
) where

type ModuleID = String
type VarName  = String

data Module = Module ModuleID [Definition]
  deriving (Show,Eq)

data Definition = DLet String Expr
		| DFun  String [String] Expr
  deriving (Show,Eq)
		
data ELit = Number Integer
	  | Bool Bool
	  | Unit 
  deriving (Show,Eq)
	  
data Operator = Internal String
  deriving (Show,Eq)

	  
data Expr = Let [(VarName,Expr)] Expr
	  | If Expr Expr Expr
	  | When Expr Expr
	  | Lit ELit
	  | Var VarName
	  | InfixC Expr Operator Expr
	  | Prefix VarName [Expr]
  deriving (Show,Eq)