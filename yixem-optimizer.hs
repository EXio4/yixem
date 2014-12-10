module Yixem.Optimizer (
  Priority,
  optNone,
  optAll,
  optimizer
) where

import Yixem.AST
import Yixem.Typechecker
import Yixem.Optimizer.Types

-- phases
import Yixem.Optimizer.LambdaLift

import Data.Foldable
import Control.Monad.Trans.Except
import Control.Applicative

phases :: [CompilerPhase]
phases = [
    lambdalift
  ]

runPhase :: Module -> CompilerPhase -> Except String Module
runPhase p (CPhase _ n f) =
  let newp = f p in
  case typecheck newp of
    Nothing -> return newp
    Just _  -> throwE n

optimizer :: Priority -> Module -> Either String Module
optimizer n p = runExcept $ foldlM runPhase p (filter (\x -> phasePriority x <= n) phases)