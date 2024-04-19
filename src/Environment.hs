module Environment () where

import Compile
  ( RuleDefinition (..),
    compileRule,
  )
import Data.Graph.Inductive (Context, DynGraph ((&)), Graph (empty, mkGraph), Node)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Error (CompileResult)
import Predicate (IndexedPredicate)
import Stmt (Stmt)

data Environment = Environment
  { _graph :: !(Gr [Stmt Int] [IndexedPredicate]),
    _start :: !Node
  }
  deriving (Show, Eq)

createEnvironmentFromString :: String -> CompileResult Environment
createEnvironmentFromString str = undefined
  

createEnvironment :: [RuleDefinition] -> CompileResult Environment
createEnvironment rules = do
  rules' <- mapM compileRule rules
  let start = 0
  let lnodes = (start, []) : zip [(start + 1) ..] (map snd rules')
  let ledges = zipWith (\i p -> (start, i, p)) [(start + 1) ..] (map fst rules')
  let gr = mkGraph lnodes ledges
  error $ show gr
  Right $ Environment gr start