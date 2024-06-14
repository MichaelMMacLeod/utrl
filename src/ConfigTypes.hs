module ConfigTypes (Config (..)) where
import Data.Kind (Type)

type Config :: Type
data Config = Config
  { definitions :: FilePath,
    input :: Maybe FilePath,
    trace :: Bool
  }