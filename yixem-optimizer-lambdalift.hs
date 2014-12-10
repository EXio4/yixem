module Yixem.Optimizer.LambdaLift (
  lambdalift
) where

import Yixem.AST
import Yixem.Optimizer.Types

trans :: Module -> Module
trans x = x

lambdalift :: CompilerPhase
lambdalift = CPhase optNone "lambda-lifting" trans