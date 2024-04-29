module AstC2Value
  ( Value (..),
    expectNat,
    expectBool,
    expectAst,
    mkTypeError,
  )
where

import qualified Ast0

data Value
  = Nat Int
  | Bool Bool
  | Ast Ast0.Ast
  deriving (Show, Eq)

mkTypeError :: String -> Value -> a
mkTypeError t x = error $ "expected " ++ t ++ ", got " ++ show x

expectNat :: Value -> Int
expectNat (Nat n) =
  if n < 0
    then error $ "nat was negative: " ++ show n
    else n
expectNat x = mkTypeError "Nat" x

expectBool :: Value -> Bool
expectBool (Bool b) = b
expectBool x = mkTypeError "Bool" x

expectAst :: Value -> Ast0.Ast
expectAst (Ast a) = a
expectAst x = mkTypeError "Ast" x