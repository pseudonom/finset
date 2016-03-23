{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Lib where

import Data.Proxy
import GHC.TypeLits
import Numeric.Natural

data Fin (n :: Nat) where
  FZ :: Fin (k + 1)
  FS :: Fin k -> Fin (k + 1)
deriving instance Show (Fin n)

finToNat :: Fin n -> Natural
finToNat FZ = 0
finToNat (FS k) = 1 + finToNat k

class NatToFin (c :: Nat) where
  natToFin :: (KnownNat c) => Natural -> Maybe (Fin c)
instance {-# OVERLAPPING #-} NatToFin 0 where
  natToFin _ = Nothing
instance (c ~ (p + 1), KnownNat p, NatToFin p) => NatToFin c where
  natToFin 0 = Just FZ
  natToFin i = FS <$> natToFin (i - 1)
