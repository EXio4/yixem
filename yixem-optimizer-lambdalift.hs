module Yixem.Optimizer.LambdaLift (
  lambdalift
) where

-- lambda lifting doesn't care about shadowed variables
--  it needs to run after the "shadowing elimination" phase

import Yixem.AST
import Yixem.Optimizer.Types

import Control.Arrow

transp :: Module -> Module
transp (Module m xs) = Module m (concatMap ldef xs)

-- repeatdly "lambda-lift" until nothing more can be done
trans :: Module -> Module
trans x | x == newx = newx
	| otherwise = trans newx
  where newx = transp x	

replace :: String -> String -> Expr -> Expr
replace s new e = rp e
  where
    rp e = case e of
	Let xs r ->
	  Let
	    (map (id *** rp) xs)
	    (rp r)
	If e1 e2 e3 ->
	  If (rp e1) (rp e2) (rp e3)
	When e1 e2 ->
	  When (rp e1) (rp e2)
	Lit n -> Lit n
	Var x ->
	  if (x == s)
	  then Var new
	  else Var x
	InfixC e1 o e2 ->
	  InfixC (rp e1) o (rp e2)
	Prefix x xs ->
	  Prefix
	    (if (x == s)
	     then new
             else x)
            (map rp xs)
	

name :: String -> String -> String
name f v = "#" ++ f ++ "##_" ++ v 
	
rep :: String -> [(String,Expr)] -> Expr -> [Definition]
rep m [] (Let xs r) = rep m xs r
rep m [] x = [DLet m x]
rep m ((s,e):xs) body =
    (DLet nm e):rep m (map (id *** rp) xs) (rp body)
  where 
        nm = name m s
	rp = replace s nm
    
ldef :: Definition -> [Definition]
ldef (DLet x e) =
  case e of
       Let xs r -> rep x xs r
       w        -> [DLet x e]
ldef (DFun x xs e) =
  case e of
       Let xs r -> rep x xs r
       w        -> [DFun x xs e]
	      
lambdalift :: CompilerPhase
lambdalift = CPhase optNone "lambda-lifting" trans