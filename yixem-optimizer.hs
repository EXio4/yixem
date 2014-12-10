module Yixem.Optimizer 
  (optimizer)
where

import Yixem.Parser 
import Yixem.Typechecker

import Data.Foldable

import Control.Monad.Trans.Except

data CompilerPhase =
  CPhase {
     phaseName :: String,
     phaseFunc :: Program -> Program
  }


phases :: [CompilerPhase]
phases = []

runPhase :: Program -> CompilerPhase -> Except String Program
runPhase p (CPhase n f) =
  let newp = f p in
  case typecheck newp of
    Nothing -> return newp
    Just _  -> throwE n
    

optimizer :: Program -> Either String Program
optimizer p = runExcept (foldlM runPhase p phases)