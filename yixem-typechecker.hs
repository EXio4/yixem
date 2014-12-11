module Yixem.Typechecker
  (typecheck)
  where

import Yixem.AST
import qualified Yixem.Lambda.Typechecker as L
import Yixem.Lambda.Typechecker (Type(..))

infixr 5 %>
(%>) = L.TFun

env :: [(String,Type)]
env = [
  ("+",       TInt %> TInt %> TInt),
  ("-",       TInt %> TInt %> TInt),
  ("*",       TInt %> TInt %> TInt),
  (">",       TInt %> TInt %> TBool),
  ("<",       TInt %> TInt %> TBool),
  ("/=",      TInt %> TInt %> TBool),
  ("==",      TInt %> TInt %> TBool),
  ("||",      TBool %> TBool %> TBool),
  ("&&",      TBool %> TBool %> TBool),
  ("^",       TBool %> TBool %> TBool),
  ("not",     TBool %> TBool),
  ("_when",   TBool %> TUnit %> TUnit),
  ("print",   TVar "a" %> TUnit),
  ("println", TVar "a" %> TUnit),
  ("_if",     TBool %> TVar "k" %> TVar "k" %> TVar "k"),
  ("read",    TInt),
  ("_fix",    (TVar "a" %> TVar "a") %> TVar "a")
  ]

  
cexp :: L.Exp -> Maybe String
cexp x = 
  case L.typeInf env x of
       Left x -> Just x
       Right _ -> Nothing
       
op2v :: Operator -> String
op2v (Internal x) = x


clambda :: Expr -> L.Exp
clambda (Var x) = L.EVar x
clambda (Let xs e) = foldr (\(s,b) r -> L.ELet s (clambda b) r) (clambda e) xs
clambda (Prefix x ep) = foldl L.EApp (L.EVar x) (map clambda ep)
clambda (When e1 e2)  = (L.EApp (L.EApp (L.EVar "_when") (clambda e1)) (clambda e2))
clambda (InfixC e1 op e2) = (L.EApp (L.EApp (L.EVar (op2v op)) (clambda e1)) (clambda e2))
clambda (Lit n) = L.ELit $
  case n of
       Number n -> L.LInt n
       Bool   n -> L.LBool n
       Unit     -> L.LUnit
clambda (If c e1 e2)  = (L.EApp (L.EApp (L.EApp (L.EVar "_if") (clambda c)) (clambda e1)) (clambda e2))

wrap :: (String,L.Exp) -> L.Exp -> L.Exp
wrap (s,e) r = L.ELet s (L.EApp (L.EVar "_fix") (L.EAbs s e)) r

prog :: Module -> L.Exp
prog (Module m xs) = foldr wrap (L.EVar "main") (map defExp xs)

add :: [String] -> L.Exp -> L.Exp
add xs d = foldr L.EAbs d xs

defExp :: Definition -> (String, L.Exp)
defExp (DLet v e1)    = (v,clambda e1)
defExp (DFun v vs e1) = (v, add vs (clambda e1))


typecheck :: Module -> Maybe String
typecheck = cexp . prog