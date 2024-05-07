module Config (Config (..)) where

data Config = Config
  { rules :: !FilePath,
    input :: !FilePath,
    dumpStmts :: Bool
  }