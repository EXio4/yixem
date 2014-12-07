{-# LANGUAGE FlexibleContexts #-}

module Main where

import Yixem
import YixemL

import System.Environment
import Control.Monad.State.Strict
import Data.List

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

inscope :: MonadState Vs m => String -> m Bool
inscope str = do
  Vs elements <- get
  return $ maybe False (const True) $ lookup str elements
  
 
data Fun = Function (String,String) [String]

getenv :: MonadState Vs m => Fun -> String -> m Reg
getenv (Function (m,f) xs) str =
  case find (==str) xs of
       Just x  -> varx (m ++ "#" ++ f ++ "#" ++ x)
       Nothing -> varx (m ++ "#_" ++ str)
       
----------------------------------------------------------

v :: Vars -> [String]
v (LV (Ident x)) = [x]
v (EV (Ident x) xs) = x:v xs

ag :: ExLst -> [Expr]
ag (LE x) = [x]
ag (EE x xs) = x:ag xs
 
 
 
cpProgram :: Program -> State Vs DC
cpProgram (Prog (Ident m) stms) = do
  prog <- mapM (cpStm m) stms
  let namespace = Function (m,"global") []
  x <- getenv namespace "main"  
  return $ foldr Sequ (ClExpr x) prog

cpStm :: String -> Stm -> State Vs DC
cpStm m (SVar (Ident x) expr) = do
  let namespace = Function (m,x) []
  fun  <- getenv namespace x
  body <- cpExpr 0 namespace expr
  return $ SvExpr fun body
cpStm m (SFun (Ident x) vx expr) = do
  let vs = v vx
  let namespace = Function (m,x) vs
  fun <- getenv namespace x
  _ <- mapM_ (getenv namespace) vs
  body <- cpExpr 0 namespace expr
  lstack <- mapM (fmap St2Reg . (getenv namespace)) vs
  return $ SvExpr fun (foldr Sequ body lstack)

conv :: Operator -> DC -> DC -> DC
conv OPlus x y  = Apply Plus x y
conv OMult x y  = Apply Mult x y
conv OMinus x y = Apply Minus x y
conv OEq x y    = Apply Plus (Int 1) (Apply Minus x y)
conv ONeq x y   = Apply Minus x y
conv OGt  x y   = dcif (Reg 'c') (CGt x y) (Int 0) (Int 1) -- super cool hack, (a > b) returns it the-wrong-way, 
conv OLt  x y   = dcif (Reg 'c') (CLt x y) (Int 0) (Int 1) -- ^ same

cpExpr :: Int -> Fun -> Expr -> State Vs DC
cpExpr n namespace xpr =
  case xpr of
       EUnit -> return (Int 0)
       ELit x -> return (Int x)
       EBool x -> return . Int $
	  case x of
	    VTrue  -> 1
	    VFalse -> 0
       EVar (Ident x) ->
	case x of
	  "read" -> return IORead
	  x -> fmap LoadReg (getenv namespace x)
       EOp e1 o e2 -> do
	 a <- cpExpr (n+1) namespace e1
	 b <- cpExpr (n+1) namespace e2
	 return $ conv o a b
       EWhen e1 e2 -> do
	 reg <- varx ("_whencond_" ++ show n)
	 a <- cpExpr (n+1) namespace e1
	 b <- cpExpr (n+1) namespace e2
	 return $ Branch reg (CEqu a (Int 1)) b
       EIf e1 e2 e3 -> do
	 reg <- varx ("_whencond_" ++ show n)
	 a <- cpExpr (n+1) namespace e1
	 b <- cpExpr (n+1) namespace e2
	 c <- cpExpr (n+1) namespace e3
	 return $ dcif reg (CEqu a (Int 1)) b c
       ECall (Ident x) els -> do
	 let xs = ag els
	 case x of
	    "println" ->
	      case xs of
		[y] -> fmap IOWriteNL (cpExpr (n+1) namespace y)
		_   -> error "println called with an invalid number of params"	    
	    "print" ->
	      case xs of
		[y] -> fmap IOWrite (cpExpr (n+1) namespace y)
		_   -> error "print called with an invalid number of params"
	    x -> do
	      fun <- getenv namespace x
	      b <- mapM (cpExpr (n+1) namespace) xs
	      return $ foldr Sequ (ClExpr fun) b
	      
	      
compile :: Program -> String
compile p = c_dc (evalState (cpProgram p) (Vs []))
	      
	     
main :: IO ()
main = getArgs >>= \x ->
  case x of
       [x] -> readFile x >>= \y ->
	  case parserX y of
		Right v -> putStrLn ("# file: "++x) >> putStrLn (compile v)
		Left  x -> putStrLn "Error parsing file" >> putStrLn x
       _   -> putStrLn "give it exactly one param (the file to compile)"