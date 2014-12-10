module Yixem.Optimizer.Types (
  Priority(..),
  CompilerPhase(..),
  optAll,
  optNone
) where

import Yixem.AST

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
     phaseFunc     :: Module -> Module
  }
  


optNone = Priority 0
optAll  = Priority 1 -- ?  
