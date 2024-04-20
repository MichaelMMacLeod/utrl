module Environment (createEnvironment, Environment (..)) where

import Compile
  ( RuleDefinition (..),
    compile0toRuleDefinition,
    compileRule,
  )
import Data.Graph.Inductive (Context, DynGraph ((&)), Graph (empty, mkGraph), Node)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Text (Text)
import Error (CompileResult)
import Predicate (IndexedPredicate)
import qualified Read
import Stmt (Stmt)

data Environment = Environment
  { _graph :: !(Gr [Stmt Int] [IndexedPredicate]),
    _start :: !Node
  }
  deriving (Show, Eq)

createEnvironment :: Text -> CompileResult Environment
createEnvironment text = do
  asts <- Read.read text
  rules <- mapM Compile.compile0toRuleDefinition asts
  rules' <- mapM compileRule rules
  let start = 0
  let lnodes = (start, []) : zip [(start + 1) ..] (map snd rules')
  let ledges = zipWith (\i p -> (start, i, p)) [(start + 1) ..] (map fst rules')
  let gr = mkGraph lnodes ledges
  Right $ Environment gr start