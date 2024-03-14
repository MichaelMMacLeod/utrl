{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import AstC0 qualified
import Compile qualified
import Data.HashMap.Strict qualified as H
import Display qualified
import Read qualified

-- (fn xs (flatten (list (list xs ..) ..)) -> (list xs .. ..))
main :: IO ()
main =
  putStrLn
    . Display.displayStmts
    . Compile.compile (H.fromList [("xs", [AstC0.ZeroPlus 1, AstC0.Between 1 0, AstC0.Between 1 0])])
    $ Read.read "(list xs .. ..)"