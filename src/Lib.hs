{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
module Lib where

import GHC.TypeLits
import Numeric.Natural

data Fin (n :: Nat) where
  FZ :: Fin (k + 1)
  FS :: Fin k -> Fin (k + 1)
deriving instance Show (Fin n)

finToNat :: Fin n -> Natural
finToNat FZ = 0
finToNat (FS k) = 1 + finToNat k
