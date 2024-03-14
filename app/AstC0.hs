{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module AstC0
  ( Ast (..),
    AstF (..),
    IndexC0,
    IndexElement (..),
    c1Tail,
    c0Head,
    cutC0,
    cutC0Between,
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
  | Variable IndexC0

data IndexElement
  = ZeroPlus Integer
  | LenMinus Integer
  | Between
      { zeroPlus :: Integer,
        lenMinus :: Integer
      }
  deriving (Eq)

type IndexC0 = [IndexElement]

c1Tail :: AstC0.IndexC0 -> AstC1.IndexC1
c1Tail = reverse . go . reverse
  where
    go :: AstC0.IndexC0 -> AstC1.IndexC1
    go ((AstC0.ZeroPlus i) : xs) = AstC1.ZeroPlus i : go xs
    go ((AstC0.LenMinus i) : xs) = AstC1.LenMinus i : go xs
    go _ = []

c0Head :: AstC0.IndexC0 -> AstC0.IndexC0
c0Head = reverse . go . reverse
  where
    go :: AstC0.IndexC0 -> AstC0.IndexC0
    go xs@(AstC0.Between {} : _) = xs
    go (_ : xs) = go xs
    go [] = []

cutC0 :: AstC0.IndexC0 -> (AstC0.IndexC0, AstC1.IndexC1)
cutC0 c0 = (c0Head c0, c1Tail c0)

cutC0Between :: AstC0.IndexC0 -> (AstC0.IndexC0, Maybe (Integer, Integer))
cutC0Between = go . reverse
  where
    go (AstC0.Between zp lm : others) = (reverse others, Just (zp, lm))
    go others = (others, Nothing)

data AstF r
  = SymbolF String
  | CompoundF [r]
  | VariableF IndexC0
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