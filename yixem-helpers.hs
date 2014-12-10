module Yixem.Helpers (
  prog
) where

import qualified Yixem.Parser as P
import qualified Yixem.AST    as Y

exLst :: P.ExLst -> [P.Expr]
exLst (P.LE x) = [x]
exLst (P.EE x xs) = x:exLst xs

vrLst :: P.Vars -> [String]
vrLst (P.LV (P.Ident x))    = [x]
vrLst (P.EV (P.Ident x) xs) = x:vrLst xs

prog :: P.Program -> Y.Module
prog (P.Prog (P.Ident m) stms) = Y.Module m (defs stms)

defs :: [P.Stm] -> [Y.Definition]
defs = map change
  where
    change (P.SVar (P.Ident f) e)    = Y.DLet f (expr e)
    change (P.SFun (P.Ident f) xs e) = Y.DFun f (vrLst xs) (expr e)

expr :: P.Expr -> Y.Expr
expr (P.ECall (P.Ident x) xs) =
  Y.Prefix x (map expr (exLst xs))
expr (P.ELet (P.Ident x) e1 e2) =
  Y.Let [(x,expr e1)] (expr e2)
expr (P.EWhen e1 e2)    = Y.When (expr e1) (expr e2)
expr (P.EIf e1 e2 e3)   = Y.If (expr e1) (expr e2) (expr e3)
expr (P.EOp e1 o e2)    = Y.InfixC (expr e1) (op o) (expr e2)
expr (P.EBool P.VTrue)  = Y.Lit (Y.Bool True)
expr (P.EBool P.VFalse) = Y.Lit (Y.Bool False)
expr (P.EUnit)          = Y.Lit (Y.Unit)
expr (P.ELit n)         = Y.Lit (Y.Number n)
expr (P.EVar (P.Ident x)) = Y.Var x

op :: P.Operator -> Y.Operator
op x = Y.Internal $
  case x of
       P.OPlus  -> "+"
       P.OMinus -> "-"
       P.OMult  -> "*"
       P.OEq    -> "=="
       P.ONeq   -> "/="
       P.OGt    -> ">"
       P.OLt    -> "<"
       P.OAnd   -> "&&"
       P.OOr    -> "||"
       P.OXor   -> "^"