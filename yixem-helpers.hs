module Yixem.Helpers (
  exLst,
  vrLst
) where

import Yixem.Parser

exLst :: ExLst -> [Expr]
exLst (LE x) = [x]
exLst (EE x xs) = x:exLst xs

vrLst :: Vars -> [String]
vrLst (LV (Ident x)) = [x]
vrLst (EV (Ident x) xs) = x:vrLst xs
