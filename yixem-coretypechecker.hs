module Yixem.Lambda.Typechecker
  ( Exp(..),
    Type(..),
    Lit(..),
    ti,  -- |ti :: TypeEnv -> Exp -> (Subst, Type)|
    typeInf
  ) where


import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Arrow
import Data.List
import Debug.Trace
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State


instance Show Type where
    show (TVar x) = "'" ++ x
    show TInt     = "int"
    show TBool    = "bool"
    show TUnit    = "unit"
    show (TFun x@(TFun e1 e2) e3) = "(" ++ show x ++ ") -> " ++ show e3
    show (TFun e1 e2) = show e1 ++ " -> " ++ show e2

instance Show Exp where
  show (EVar s) = s
  show (ELit x) = "L"
  show (EApp e1 e2) = "(" ++ show e1 ++ ") (" ++ show e2 ++ ")"
  show (ELet s e r) = "let " ++ s ++ " = " ++ show e ++ "\n" ++ show r
  show (EAbs s e) = "\\" ++ s ++ " -> " ++ show e 

  

data Exp     =  EVar String
             |  ELit Lit
             |  EApp Exp Exp
             |  EAbs String Exp
             |  ELet String Exp Exp
             deriving (Eq, Ord)

data Lit     =  LInt Integer
             |  LBool Bool
	     |  LUnit
             deriving (Eq, Ord)

data Type    =  TVar String
             |  TInt
             |  TBool
	     |  TUnit
             |  TFun Type Type
             deriving (Eq, Ord)

data Scheme  =  Scheme [String] Type


class Types a where
    ftv    ::  a -> Set.Set String
    apply  ::  Subst -> a -> a



instance Types Type where
    ftv (TVar n)      =  Set.singleton n
    ftv TInt          =  Set.empty
    ftv TBool         =  Set.empty
    ftv TUnit         =  Set.empty
    ftv (TFun t1 t2)  =  ftv t1 `Set.union` ftv t2

    apply s (TVar n)      =  case Map.lookup n s of
                               Nothing  -> TVar n
                               Just t   -> t
    apply s (TFun t1 t2)  = TFun (apply s t1) (apply s t2)
    apply s t             =  t



instance Types Scheme where
    ftv (Scheme vars t)      =  (ftv t) `Set.difference` (Set.fromList vars)

    apply s (Scheme vars t)  =  Scheme vars (apply (foldr Map.delete s vars) t)


instance Types a => Types [a] where
    apply s  =  map (apply s)
    ftv l    =  foldr Set.union Set.empty (map ftv l)



type Subst = Map.Map String Type

nullSubst  ::  Subst
nullSubst  =   Map.empty

composeSubst         :: Subst -> Subst -> Subst
composeSubst s1 s2   = (Map.map (apply s1) s2) `Map.union` s1

newtype TypeEnv = TypeEnv (Map.Map String Scheme)

remove                    ::  TypeEnv -> String -> TypeEnv
remove (TypeEnv env) var  =  TypeEnv (Map.delete var env)

instance Types TypeEnv where
    ftv (TypeEnv env)      =  ftv (Map.elems env)
    apply s (TypeEnv env)  =  TypeEnv (Map.map (apply s) env)


generalize        ::  TypeEnv -> Type -> Scheme
generalize env t  =   Scheme vars t
  where vars = Set.toList ((ftv t) `Set.difference` (ftv env))

data TIEnv = TIEnv  {}

data TIState = TIState {  tiSupply :: Int,
                          tiSubst :: Subst}

type TI a = ErrorT String (ReaderT TIEnv (StateT TIState IO)) a

runTI :: TI a -> IO (Either String a, TIState)
runTI t = 
    do (res, st) <- runStateT (runReaderT (runErrorT t) initTIEnv) initTIState
       return (res, st)
  where initTIEnv = TIEnv{}
        initTIState = TIState{tiSupply = 0,
                              tiSubst = Map.empty}

newTyVar :: String -> TI Type
newTyVar prefix =
    do  s <- get
        put s{tiSupply = tiSupply s + 1}
        return (TVar  (prefix ++ show (tiSupply s)))

instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do  nvars <- mapM (\ _ -> newTyVar "a") vars
                                  let s = Map.fromList (zip vars nvars)
                                  return $ apply s t

mgu :: Type -> Type -> TI Subst
mgu (TFun l r) (TFun l' r')  =  do  s1 <- mgu l l'
                                    s2 <- mgu (apply s1 r) (apply s1 r')
                                    return (s1 `composeSubst` s2)
mgu (TVar u) t               =  varBind u t
mgu t (TVar u)               =  varBind u t
mgu TInt TInt                =  return nullSubst
mgu TBool TBool              =  return nullSubst
mgu t1 t2                    =  throwError $ "types do not unify: " ++ show t1 ++ 
                                " vs. " ++ show t2

varBind :: String -> Type -> TI Subst
varBind u t  | t == TVar u           =  return nullSubst
             | u `Set.member` ftv t  =  throwError $ "occur check fails: '" ++ u ++
                                         " vs. " ++ show t
             | otherwise             =  return (Map.singleton u t)


tiLit :: TypeEnv -> Lit -> TI (Subst, Type)
tiLit _ (LInt _)   =  return (nullSubst, TInt)
tiLit _ (LBool _)  =  return (nullSubst, TBool)
tiLit _ (LUnit)    =  return (nullSubst, TUnit)


ti        ::  TypeEnv -> Exp -> TI (Subst, Type)
ti (TypeEnv env) (EVar n) = 
    case Map.lookup n env of
       Nothing     ->  throwError $ "unbound variable: " ++ n
       Just sigma  ->  do  t <- instantiate sigma
                           return (nullSubst, t)
ti env (ELit l) = tiLit env l
ti env (EAbs n e) =
    do  tv <- newTyVar "a"
        let TypeEnv env' = remove env n
            env'' = TypeEnv (env' `Map.union` (Map.singleton n (Scheme [] tv)))
        (s1, t1) <- ti env'' e
        return (s1, TFun (apply s1 tv) t1)
ti env (EApp e1 e2) =
    do  tv <- newTyVar "a"
        (s1, t1) <- ti env e1
        (s2, t2) <- ti (apply s1 env) e2
        s3 <- mgu (apply s2 t1) (TFun t2 tv)
        return (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)
ti env (ELet x e1 e2) =
    do  (s1, t1) <- ti env e1
        let TypeEnv env' = remove env x
            t' = generalize (apply s1 env) t1
            env'' = TypeEnv (Map.insert x t' env')
        (s2, t2) <- ti (apply s1 env'') e2
        return (s1 `composeSubst` s2, t2)


        
typeInference :: Map.Map String Scheme -> Exp -> TI Type
typeInference env e =
    do  (s, t) <- ti (TypeEnv env) e
        return (apply s t)
        
toschm :: Type -> Scheme
toschm (TVar x) = Scheme [x] (TVar x)
toschm TInt     = Scheme []   TInt
toschm TBool    = Scheme []   TBool
toschm TUnit    = Scheme []   TUnit
toschm (TFun x y) =
  let (Scheme xs t1) = toschm x
      (Scheme ys t2) = toschm y
  in Scheme (nub (xs ++ ys)) (TFun t1 t2)
 
conv :: [(String,Type)] -> [(String,Scheme)]
conv = map (id *** toschm)
        
typeInf :: [(String,Type)] -> Exp -> IO (Either String Type)
typeInf ev e = do
  let env = Map.fromList (conv ev)
  (res,_) <-runTI (typeInference env e)
  return res
	


