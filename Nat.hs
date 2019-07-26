{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Nat where

import Compare (Compare)
import Data.Type.Equality

-- Natural Numbers.
data Nat = Z | S Nat
  deriving (Eq, Ord, Show)

-- Singleton for Natural Numbers.
data Natty :: Nat -> * where
  Zy :: Natty 'Z
  Sy :: Natty n -> Natty ('S n)
deriving instance Eq (Natty n)

natty2Int :: Natty n -> Int
natty2Int n = natty2IntAc n 0

natty2IntAc :: Natty n -> Int -> Int
natty2IntAc Zy     ac = ac
natty2IntAc (Sy n) ac = natty2IntAc n (ac+1)

instance Show (Natty n) where
  show n = show $ natty2Int n

data OWOTO :: Nat -> Nat -> * where
  LE :: (Compare x y ~ 'LT) => OWOTO x y
  EE :: (Compare x x ~ 'EQ) => OWOTO x x
  GE :: (Compare x y ~ 'GT) => OWOTO x y

owoto :: Natty m -> Natty n -> OWOTO m n
owoto Zy      Zy      = EE
owoto Zy      (Sy _)  = LE
owoto (Sy _)  Zy      = GE
owoto (Sy m)  (Sy n)  = case owoto m n of
  LE -> LE
  GE -> GE
  EE -> EE

type family CompareNat (m :: Nat) (n :: Nat) :: Ordering where
  CompareNat 'Z      'Z      = 'EQ
  CompareNat ('S m)  ('S n)  = CompareNat m n
  CompareNat ('S m)  'Z      = 'GT
  CompareNat 'Z      ('S n)  = 'LT
type instance Compare (a :: Nat) (b :: Nat) = CompareNat a b
