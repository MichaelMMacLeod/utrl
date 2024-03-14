{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Compile (compile) where

import Ast0 qualified
import Ast1 qualified
import AstC0 qualified
import AstC1 qualified
import Data.Functor.Foldable (Base, cata)
import Data.HashMap.Strict qualified as H

type Variables = H.HashMap String AstC0.Index

compile :: Variables -> Ast0.Ast -> AstC1.Ast
compile vars = compileC0toC1 . compile1toC0 vars . compile0to1

compile0to1 :: Ast0.Ast -> Ast1.Ast
compile0to1 = cata $ \case
  Ast0.SymbolF s -> Ast1.Symbol s
  Ast0.CompoundF xs -> Ast1.Compound $ go xs
    where
      go :: [Ast1.Ast] -> [Ast1.Ast]
      go (y : Ast1.Symbol ".." : ys) = go $ Ast1.Ellipses y : ys
      go (y : ys) = y : go ys
      go [] = []

compile1toC0 :: Variables -> Ast1.Ast -> AstC0.Ast
compile1toC0 vars = cata $ \case
  Ast1.SymbolF s -> case H.lookup s vars of
    Nothing -> AstC0.Symbol s
    Just index -> AstC0.Variable index
  Ast1.CompoundF xs -> AstC0.Compound xs
  Ast1.EllipsesF x -> AstC0.Ellipses x

compileC0toC1 :: AstC0.Ast -> AstC1.Ast
compileC0toC1 = verify . cata go
  where
    verify :: (AstC1.Ast, AstC0.Index) -> AstC1.Ast
    verify (ast, []) = ast
    verify _ = error "Needs more '..'"

    go :: Base AstC0.Ast (AstC1.Ast, AstC0.Index) -> (AstC1.Ast, AstC0.Index)
    go (AstC0.SymbolF s) = (AstC1.Symbol s, [])
    go (AstC0.CompoundF xs) =
      let indexesAllEqual = allEqual $ map snd xs
          allEqual :: [AstC0.Index] -> Bool
          allEqual [] = True
          allEqual (y : ys) = all (== y) ys
          sharedIndex :: [(AstC1.Ast, AstC0.Index)] -> AstC0.Index
          sharedIndex ((_, i) : _) = i
          sharedIndex _ = []
       in if indexesAllEqual
            then (AstC1.Compound $ map fst xs, sharedIndex xs)
            else error "Variables not matched under same '..' used under same '..'"
    go (AstC0.VariableF i) =
      let (c0Part, c1Part) = AstC0.cutC0 i
       in (AstC1.Copy c1Part, c0Part)
    go (AstC0.EllipsesF (astC1, indexC0)) = case AstC0.cutC0Between indexC0 of
      (indexC0', Just (zeroPlus, lenMinus)) ->
        let (fstC0, sndC1) = AstC0.cutC0 indexC0'
            loopC1 = AstC1.Loop {AstC1.index = sndC1, AstC1.start = zeroPlus, AstC1.end = lenMinus, AstC1.body = astC1}
         in (loopC1, fstC0)
      (_, Nothing) -> error "Too many '..'"
