{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Nat where

import Compare (Compare, OWOTO(..))

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

owotoNat :: Natty m -> Natty n -> OWOTO m n
owotoNat Zy      Zy      = EE
owotoNat Zy      (Sy _)  = LE
owotoNat (Sy _)  Zy      = GE
owotoNat (Sy m)  (Sy n)  = case owotoNat m n of
  LE -> LE
  GE -> GE
  EE -> EE

type family CompareNat (m :: Nat) (n :: Nat) :: Ordering where
  CompareNat 'Z      'Z      = 'EQ
  CompareNat ('S m)  ('S n)  = CompareNat m n
  CompareNat ('S m)  'Z      = 'GT
  CompareNat 'Z      ('S n)  = 'LT
type instance Compare (a :: Nat) (b :: Nat) = CompareNat a b
