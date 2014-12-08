module Yixem.CoreLang
  (
    Cond(..),
    DC(..),
    Reg(..),
    Opt(..),
    dcif,
    c_dc
  )where

data Opt = Plus | Minus | Mult deriving (Show,Eq)
data Reg = Reg Char deriving (Show,Eq)

data Cond =
    CEqu DC DC
  | CNeq DC DC
  | CLt  DC DC
  | CEGt DC DC
  | CGt  DC DC
  | CELt DC DC
  deriving (Show,Eq)
  
-- "negation" of a conditional
inv :: Cond -> Cond
inv (CEqu x y) = CNeq x y
inv (CNeq x y) = CEqu x y
inv (CLt  x y) = CEGt x y
inv (CEGt x y) = CLt  x y
inv (CGt  x y) = CELt x y
inv (CELt x y) = CGt  x y

data DC =
    LoadReg Reg
  | SaveReg Reg DC
  | Apply Opt DC DC
  | Str String
  | Int Integer
  | IORead
  | IOWrite DC   
  | IOWriteNL DC
  | Quit Integer -- integer = number of levels to "quit"
  | Branch Reg Cond DC  
  | Sequ DC DC    -- sequencing operator            
  | SvExpr Reg DC -- save expression into "reg" 
  | ClExpr Reg    -- load and execute "reg"
  | St2Reg Reg
  deriving (Show,Eq)

c_dc_op Plus  = "+"
c_dc_op Minus = "-"
c_dc_op Mult  = "*"

c_dc_reg (Reg x) = [x]

condh x y = c_dc x ++ " " ++ c_dc y ++ " "
c_dc_cond (CEqu x y) = condh x y ++ " ="
c_dc_cond (CNeq x y) = condh x y ++ " !="
c_dc_cond (CLt  x y) = condh x y ++ " <"
c_dc_cond (CEGt x y) = condh x y ++ " !<"
c_dc_cond (CGt  x y) = condh x y ++ " >"
c_dc_cond (CELt x y) = condh x y ++ " !>"

dcif r c e1 e2 =  Sequ 
    (Branch r      c  e1)
    (Branch r (inv c) e2)
  

c_dc :: DC -> String
c_dc (IORead)         = "?"
c_dc (IOWrite   e)    = c_dc e ++ " n"
c_dc (IOWriteNL e)    = c_dc e ++ " p sv"
c_dc (Int x)  | x < 0 = "_" ++ show (abs x) ++ " "
c_dc (Int x)          = show x ++ " "
c_dc (Str x)          = "[" ++ x ++ "]"
c_dc (Apply op x y)   = c_dc x ++ " " ++ c_dc y ++ c_dc_op op
c_dc (LoadReg r)      = "l" ++ c_dc_reg r
c_dc (SaveReg r e)    = c_dc e ++ " s" ++ c_dc_reg r
c_dc (St2Reg  r)      = " s" ++ c_dc_reg r
c_dc (SvExpr r e)     = "[" ++ c_dc e ++ "] d s" ++ c_dc_reg r
c_dc (ClExpr r)       = "l" ++ c_dc_reg r ++ " x"
c_dc (Quit n)         = show n ++ "Q"
c_dc (Sequ x y)       = c_dc x ++ "\n" ++ c_dc y
c_dc (Branch r c e)   = "[" ++ c_dc e ++ "] s" ++ c_dc_reg r ++ " " ++ c_dc_cond c ++ c_dc_reg r


----------------------