module Environment (createEnvironment, Environment (..)) where

import qualified AstC2
import Compile
  ( compile0toRuleDefinition,
    compileRule2,
  )
import Data.Graph.Inductive (Graph (mkGraph), Node)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Text (Text)
import Error (CompileResult)
import Predicate (IndexedPredicate)
import qualified Read

data Environment = Environment
  { _graph :: !(Gr (AstC2.Ast Int) [IndexedPredicate]),
    _start :: !Node
  }
  deriving (Show, Eq)

createEnvironment :: Text -> CompileResult Environment
createEnvironment text = do
  asts <- Read.read text
  rules <- mapM Compile.compile0toRuleDefinition asts
  rules' <- mapM compileRule2 rules
  let start = 0
  let lnodes = (start, []) : zip [(start + 1) ..] (map snd rules')
  let ledges = zipWith (\i p -> (start, i, p)) [(start + 1) ..] (map fst rules')
  let gr = mkGraph lnodes ledges
  Right $ Environment gr start