module Yixem.Typechecker
  (typecheck)
  where
import Debug.Trace
import Yixem.Parser
import Yixem.Compiler (exLst,vrLst)
import qualified Yixem.Lambda.Typechecker as L
import Yixem.Lambda.Typechecker (Type(..))

infixr 5 %>
(%>) = L.TFun

env :: [(String,Type)]
env = [
  ("+", TInt %> TInt %> TInt),
  ("-", TInt %> TInt %> TInt),
  ("*", TInt %> TInt %> TInt),
  (">", TInt %> TInt %> TBool),
  ("<", TInt %> TInt %> TBool),
  ("/=", TInt %> TInt %> TBool),
  ("==", TInt %> TInt %> TBool),
  ("||", TBool %> TBool %> TBool),
  ("&&", TBool %> TBool %> TBool),
  ("^",  TBool %> TBool %> TBool),
  ("not", TBool %> TBool),
  ("_when",  TBool %> TUnit %> TUnit),
  ("print", TVar "a" %> TUnit),
  ("println", TVar "a" %> TUnit),
  ("_if", TBool %> TVar "k" %> TVar "k" %> TVar "k"),
  ("read", TInt),
  ("_fix", (TVar "a" %> TVar "a") %> TVar "a")
  ]

  
cexp :: L.Exp -> IO Bool
cexp x = L.typeInf env x >>= \y ->
  case y of
       Left  z -> putStrLn ("error typechecking\n\t" ++ z) >> return False
       Right z -> return True
       
op2v :: Operator -> String
op2v OPlus  = "+"
op2v OMinus = "-"
op2v OMult  = "*"
op2v OEq    = "=="
op2v ONeq   = "/="
op2v OLt    = "<"
op2v OGt    = ">"
op2v OOr    = "||"
op2v OAnd   = "&&"
op2v OXor   = "^"	


clambda :: Expr -> L.Exp
clambda (EVar (Ident x)) = L.EVar x
clambda (ECall (Ident x) ep) = foldl L.EApp (L.EVar x) (map clambda (exLst ep))
clambda (EWhen e1 e2)  = (L.EApp (L.EApp (L.EVar "_when") (clambda e1)) (clambda e2))
clambda (EOp e1 op e2) = (L.EApp (L.EApp (L.EVar (op2v op)) (clambda e1)) (clambda e2))
clambda EBool{}        = L.ELit (L.LBool undefined)
clambda ELit{}         = L.ELit (L.LInt  undefined)
clambda EUnit          = L.ELit L.LUnit
clambda (EIf c e1 e2)  = (L.EApp (L.EApp (L.EApp (L.EVar "_if") (clambda c)) (clambda e1)) (clambda e2))

wrap :: (String,L.Exp) -> L.Exp -> L.Exp
wrap (s,e) r = L.ELet s (L.EApp (L.EVar "_fix") (L.EAbs s e)) r

prog :: Program -> L.Exp
prog (Prog (Ident m) xs) = foldr wrap (L.EVar "main") (map stmExp xs)

add :: [String] -> L.Exp -> L.Exp
add xs d = foldr L.EAbs d xs

stmExp :: Stm -> (String, L.Exp)
stmExp (SVar (Ident v) e1) = (v,clambda e1)
stmExp (SFun (Ident v) vs e1) = (v, add (vrLst vs) (clambda e1))


typecheck :: Program -> IO Bool
typecheck = cexp . prog