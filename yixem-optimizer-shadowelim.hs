module Yixem.Optimizer.ShadowElim (
  shadowelim
) where
  
import Yixem.AST
import Yixem.Optimizer.Types
  

trans :: Module -> Module
trans (Module m xs) = Module m $ map elimShadow xs

subst :: String -> String -> Expr -> Expr
subst old new e = f e
  where
    f (Let ((s,e1):xs) e2) | s /= old  = Let [(s,f e1)] (f (Let xs e2))
			   | otherwise = Let [(new ++ "$",f e1)] (subst old (new ++ "$") e2)
    f (Let [] e) = f e
    f (If e1 e2 e3) = If (f e1) (f e2) (f e3)
    f (When e1 e2) = When (f e1) (f e2)
    f (Var x) | x == old  = Var new
    f (Prefix x xs) | x == old = Prefix new (map f xs)
    f (InfixC e1 o e2) = InfixC (f e1) o (f e2)
    f x = x

go :: Expr -> Expr
go (Let [] e) = go e
go (Let ((s,e1):xs) e2) = Let [(s,e1)] (go (subst s s (Let xs e2)))
go (If e1 e2 e3) = If (go e1) (go e2) (go e3)
go (When e1 e2)  = When (go e1) (go e2)
go (Var x) = Var x
go (Lit n) = Lit n
go (Prefix x xs) = Prefix x (map go xs)
go (InfixC e1 o e2) = InfixC (go e1) o (go e2)
    
elimShadow (DLet s e)    = DLet s (go e)
elimShadow (DFun f ps e) = DFun f ps (go e)
  
shadowelim :: CompilerPhase
shadowelim = CPhase optNone "shadowing elimination" trans