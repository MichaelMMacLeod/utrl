module ConfigTypes (Config (..)) where

data Config = Config
  { definitions :: FilePath,
    input :: Maybe FilePath,
    trace :: Bool
  }