{-# LANGUAGE FlexibleContexts #-}

module Yixem.Compiler (
  compile
) where

import Yixem.AST
import Yixem.CoreLang

import Control.Monad.State.Strict
import Data.List
import Control.Applicative

data Vs = Vs [(String,Reg)]

instance Show Vs where
  show (Vs x) = show $ map (\(a,(Reg b)) -> (a, b)) x

varx :: MonadState Vs m => String -> m Reg
varx str = do
  Vs elements <- get
  case lookup str elements of
       Just x  -> return x
       Nothing -> 
	case elements of
	    [] -> let def = Reg 'f' in
		put (Vs [(str, def)]) >>
		return def
	    xs@((_,(Reg r)):_) -> let nreg = Reg (succ r) in
		put (Vs ((str,nreg):xs)) >>
		return nreg

----------------------------------------------------------

data Fun = Function (String,String) [String]

getenv :: MonadState Vs m => Fun -> String -> m Reg
getenv (Function (m,f) xs) str =
  case find (==str) xs of
       Just x  -> varx (m ++ "#" ++ f ++ "#" ++ x)
       Nothing -> varx (m ++ "#_" ++ str)
       
----------------------------------------------------------

 
 
cpProgram :: Module -> State Vs DC
cpProgram (Module m defs) = do
  prog <- mapM (cpDefs m) defs
  let namespace = Function (m,"global") []
  x <- getenv namespace "main"  
  return $ foldr Sequ (ClExpr x) prog

cpDefs :: String -> Definition -> State Vs DC
cpDefs m (DLet x expr) = do
  let namespace = Function (m,x) []
  fun  <- getenv namespace x
  body <- cpExpr 0 namespace expr
  return $ SvExpr fun body
cpDefs m (DFun x vx expr) = do
  let namespace = Function (m,x) vx
  fun <- getenv namespace x
  _ <- mapM_ (getenv namespace) vx
  body <- cpExpr 0 namespace expr
  lstack <- mapM (fmap St2Reg . (getenv namespace)) vx
  return $ SvExpr fun (foldr Sequ body lstack)

conv :: Operator -> DC -> DC -> DC
conv (Internal c) x y =
  case c of
       "+" -> Apply Plus  x y
       "-" -> Apply Minus x y
       "*" -> Apply Mult  x y
       "==" -> Apply Plus (Int 1) (Apply Minus x y)
       "/=" -> Apply Minus x y
       ">"  -> dcif (Reg 'c') (CGt x y) (Int 0) (Int 1) -- super cool hack, (a > b) returns it the-wrong-way, 
       "<"  -> dcif (Reg 'c') (CLt x y) (Int 0) (Int 1) -- ^ same
       "&&" -> Apply Mult x y
       "||" -> Apply Minus (Apply Plus x y) (Apply Mult x y)
       "^"  -> Apply Mult (Apply Minus (Apply Plus x y) (Apply Mult x y)) (Apply Minus (Int 1) (Apply Mult x y))
       

cpExpr :: Int -> Fun -> Expr -> State Vs DC
cpExpr n namespace xpr =
  case xpr of
       Let _ _ -> error "let expression not supported by compiler [needs optimization phase]"
       Lit x -> return . Int $
	case x of
	  Unit       -> 0
	  Number n   -> n
	  Bool True  -> 1
	  Bool False -> 0
       Var x ->
	case x of
	  "read" -> return IORead
	  x -> LoadReg <$> getenv namespace x
       InfixC e1 o e2 -> 
	 conv o <$> cpExpr (n+1) namespace e1 <*> cpExpr (n+1) namespace e2
       When e1 e2 -> 
	 (\r a b -> Branch r (CEqu a (Int 1)) b)
	   <$> varx ("_whencond_" ++ show n)
	   <*> cpExpr (n+1) namespace e1
	   <*> cpExpr (n+1) namespace e2
       If e1 e2 e3 -> 
	 (\r a b c -> dcif r (CEqu a (Int 1)) b c)
	  <$> varx ("_whencond_" ++ show n)
	  <*> cpExpr (n+1) namespace e1
	  <*> cpExpr (n+1) namespace e2
	  <*> cpExpr (n+1) namespace e3
       Prefix x xs ->
	 case x of
	    "not" ->
	      case xs of
		[y] ->  Apply Minus (Int 1) <$> (cpExpr (n+1) namespace y)
		_ -> error "not called with an invalid number of params"
	    "println" ->
	      case xs of
		[y] -> IOWriteNL <$> cpExpr (n+1) namespace y
		_   -> error "println called with an invalid number of params"	    
	    "print" ->
	      case xs of
		[y] -> IOWrite <$> cpExpr (n+1) namespace y
		_   -> error "print called with an invalid number of params"
	    x ->
	      (\f -> foldr Sequ (ClExpr f))
		<$> getenv namespace x
		<*> mapM (cpExpr (n+1) namespace) xs
	      
	      
compile :: Module -> String
compile p = c_dc (evalState (cpProgram p) (Vs []))
	   
