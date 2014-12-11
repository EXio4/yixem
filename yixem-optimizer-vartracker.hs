module Yixem.Optimizer.VarTracker (
  vartracker
) where

import Yixem.AST
import Yixem.Optimizer.Types

import Control.Arrow

trans :: Module -> Module
trans (Module m xs) = Module m (rep xs)

replaceExpr :: String -> String -> Expr -> Expr
replaceExpr old new e = f e
  where
    f (Let xs e) = Let (map (id *** f) xs) (f e)
    f (If e1 e2 e3) = If (f e1) (f e2) (f e3)
    f (When e1 e2) = When (f e1) (f e2)
    f (InfixC e1 o e2) = InfixC (f e1) o (f e2)
    f (Prefix x xs) = Prefix x (map f xs)
    f (Lit n) = Lit n
    f (Var x) | x == old  = Var new
	      | otherwise = Var x


replace :: String -> String -> Definition -> Definition
replace old new (DLet x e)    = DLet x    $ replaceExpr old new e
replace old new (DFun x xs e) = DFun x xs $ replaceExpr old new e
 
 
rep :: [Definition] -> [Definition]
rep [] = []
rep (x:xs) =
  case x of
       DLet x (Var y) -> map (replace x y) (rep xs)
       w  -> w:rep xs

vartracker :: CompilerPhase
vartracker = CPhase optNone "variable-tracker" trans