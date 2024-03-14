module Stmt where

data ConstantExpr
  = VarCE Integer
  | ConstantCE Integer

data BinOp = AddOp | SubOp

data Expr
  = ConstantExprE ConstantExpr
  | -- Computes lhs_varE <opE> rhsE, storing the result in lhs_varE.
    BinOpExpr {opE :: BinOp, lhs_varE :: Integer, rhsE :: ConstantExpr}
  | -- Computes the length of the compound term in the input pointed at
    -- by the index stack, storing the result in 'length_varE'.
    LengthOp {length_varE :: Integer}

data Stmt
  = -- Evaluates rhsS and assigns it to lhs_varS
    AssignS {lhs_varS :: Integer, rhsS :: Expr}
  | -- Pushes a string to the top of the data stack
    PushSymbolS String
  | -- Pushes a constant to the top of the index stack
    PushIndexS ConstantExpr
  | -- Removes the first 'count' indices off the index stack.
    PopIndexS {count :: Integer}
  | -- Coppies the portion of the input pointed to by the index stack,
    -- pusing the copied value onto the top of the data stack.
    Copy
  | -- Pops 'length' number of elements off the top of the data stack,
    -- builds a compound term containing them, and pushes the new compound
    -- term back on top of the data stack.
    Build {lengthS :: Expr}
  | -- Jumps to instruction #labelS when 'when_var' is less than 'le_var',
    -- otherwise control continues to the next statement.
    ConditionalJump {labelS :: Integer, when_var :: Integer, le_var :: Integer}
  | -- Jumps always to a specific instruction.
    UnconditionalJump Integer
