module ReadTypes
  ( SrcLocked,
    SrcLockedF,
  )
where

import Control.Comonad.Cofree (Cofree)
import Control.Comonad.Trans.Cofree (CofreeF)
import Data.Functor.Foldable (Base)
import ErrorTypes (Span)

type SrcLocked t = Cofree (Base t) (Span Int)

type SrcLockedF t = CofreeF (Base t) (Span Int)