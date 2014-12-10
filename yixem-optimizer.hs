module Yixem.Optimizer (
  Priority,
  optNone,
  optAll,
  optimizer
) where

import Yixem.Parser 
import Yixem.Typechecker

import Data.Foldable

import Control.Monad.Trans.Except
import Control.Applicative

newtype Priority = Priority Int deriving (Show,Eq,Ord)

{-
  Priorities:
    0 = only needed for compiling the program (possible lambda lifting/defunctionalization?)
    1 = elimination of dead code?
    ... ?
-}

data CompilerPhase =
  CPhase {
     phasePriority :: Priority,
     phaseName     :: String,
     phaseFunc     :: Program -> Program
  }


phases :: [CompilerPhase]
phases = []

runPhase :: Program -> CompilerPhase -> Except String Program
runPhase p (CPhase _ n f) =
  let newp = f p in
  case typecheck newp of
    Nothing -> return newp
    Just _  -> throwE n


optNone = Priority 0
optAll  = Priority 1 -- ?

optimizer :: Priority -> Program -> Either String Program
optimizer n p = runExcept $ foldlM runPhase p (filter (\x -> phasePriority x <= n) phases)