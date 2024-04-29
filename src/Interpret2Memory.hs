module Interpret2Memory (Memory (..)) where

import qualified Ast0
import qualified AstC2
import AstC2Value (Value)

data Memory = Memory
  { input :: !Ast0.Ast,
    program :: !(AstC2.Ast Int),
    instruction :: !Int,
    dataStack :: ![Ast0.Ast],
    variables :: ![Value]
  }
  deriving (Show)