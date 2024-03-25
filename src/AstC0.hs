{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module AstC0
  ( Ast (..),
    AstF (..),
    Index,
    IndexElement (..),
    c1Tail,
    c0Head,
    popTrailingC1Index,
    popBetweenTail,
  )
where

import AstC1 qualified
import Data.Functor.Foldable
  ( Base,
    Corecursive,
    Recursive,
    embed,
    project,
  )

data Ast
  = Symbol String
  | Compound [Ast]
  | Ellipses Ast
  | Variable Index

data IndexElement
  = ZeroPlus Int
  | LenMinus Int
  | Between
      { zeroPlus :: Int,
        lenMinus :: Int
      }
  deriving (Eq)

type Index = [IndexElement]

c1Tail :: Index -> AstC1.Index
c1Tail = reverse . go . reverse
  where
    go :: Index -> AstC1.Index
    go ((AstC0.ZeroPlus i) : xs) = AstC1.ZeroPlus i : go xs
    go ((AstC0.LenMinus i) : xs) = AstC1.LenMinus i : go xs
    go _ = []

c0Head :: Index -> Index
c0Head = reverse . go . reverse
  where
    go :: Index -> Index
    go xs@(AstC0.Between {} : _) = xs
    go (_ : xs) = go xs
    go [] = []

popTrailingC1Index :: Index -> (Index, AstC1.Index)
popTrailingC1Index c0 = (c0Head c0, c1Tail c0)

popBetweenTail :: Index -> (Index, Maybe (Int, Int))
popBetweenTail = go . reverse
  where
    go (AstC0.Between zp lm : others) = (reverse others, Just (zp, lm))
    go others = (others, Nothing)

data AstF r
  = SymbolF String
  | CompoundF [r]
  | VariableF Index
  | EllipsesF r
  deriving (Functor)

type instance Base Ast = AstF

instance Recursive Ast where
  project (Symbol s) = SymbolF s
  project (Compound xs) = CompoundF xs
  project (Variable i) = VariableF i
  project (Ellipses x) = EllipsesF x

instance Corecursive Ast where
  embed (SymbolF s) = Symbol s
  embed (CompoundF xs) = Compound xs
  embed (VariableF i) = Variable i
  embed (EllipsesF x) = Ellipses x