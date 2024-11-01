module ConfigTypes
  ( Config (..),
    EmitStages (..),
    onlyStage0,
    onlyStage1,
    onlyStageC0,
    onlyStageC1,
    onlyStageC2,
    onlyStageP0,
    onlyStageStmts,
  )
where

import Data.Kind (Type)
import Data.Text (Text)

type EmitStages :: Type
data EmitStages = EmitStages
  { stage0 :: Bool,
    stage1 :: Bool,
    stageC0 :: Bool,
    stageC1 :: Bool,
    stageC2 :: Bool,
    stageP0 :: Bool,
    stageStmts :: Bool
  }
  deriving stock (Show)

instance Semigroup EmitStages where
  (<>) :: EmitStages -> EmitStages -> EmitStages
  e1 <> e2 =
    EmitStages
      { stage0 = e1.stage0 || e2.stage0,
        stage1 = e1.stage1 || e2.stage1,
        stageC0 = e1.stageC0 || e2.stageC0,
        stageC1 = e1.stageC1 || e2.stageC1,
        stageC2 = e1.stageC2 || e2.stageC2,
        stageP0 = e1.stageP0 || e2.stageP0,
        stageStmts = e1.stageStmts || e2.stageStmts
      }

instance Monoid EmitStages where
  mempty :: EmitStages
  mempty =
    EmitStages
      { stage0 = False,
        stage1 = False,
        stageC0 = False,
        stageC1 = False,
        stageC2 = False,
        stageP0 = False,
        stageStmts = False
      }

onlyStage0 :: EmitStages
onlyStage0 = EmitStages True False False False False False False

onlyStage1 :: EmitStages
onlyStage1 = EmitStages False True False False False False False

onlyStageC0 :: EmitStages
onlyStageC0 = EmitStages False False True False False False False

onlyStageC1 :: EmitStages
onlyStageC1 = EmitStages False False False True False False False

onlyStageC2 :: EmitStages
onlyStageC2 = EmitStages False False False False True False False

onlyStageP0 :: EmitStages
onlyStageP0 = EmitStages False False False False False True False

onlyStageStmts :: EmitStages
onlyStageStmts = EmitStages False False False False False False True

type Config :: Type
data Config = Config
  { definitions :: FilePath,
    input :: Maybe FilePath,
    trace :: Bool,
    unparsedEmitStagesText :: Maybe Text
  }