module Yixem.Optimizer (
  Priority,
  optNone,
  optAll,
  ErrOpt(..),
  optimizer
) where

import Yixem.AST
import Yixem.Typechecker
import Yixem.Optimizer.Types

-- phases
import Yixem.Optimizer.LambdaLift
import Yixem.Optimizer.ShadowElim
import Yixem.Optimizer.VarTracker

import Data.Foldable
import Control.Monad.Trans.Except
import Control.Applicative

phases :: [CompilerPhase]
phases = [
    shadowelim,
    lambdalift,
    vartracker
  ]
  
data ErrOpt = ErrOpt {
    errPhaseName :: String,
    errTypeError :: String,
    errGenProg   :: Module
  } deriving (Show,Eq)

runPhase :: Module -> CompilerPhase -> Except ErrOpt Module
runPhase p (CPhase _ n f) =
  let newp = f p in
  case typecheck newp of
    Nothing -> return newp
    Just x  -> throwE (ErrOpt n x newp)

optimizer :: Priority -> Module -> Either ErrOpt Module
optimizer n p = runExcept $ foldlM runPhase p (filter (\x -> phasePriority x <= n) phases)