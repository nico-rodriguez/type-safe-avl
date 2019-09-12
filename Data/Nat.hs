{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

module Data.Nat where

import Data.Proxy

-- Natural Numbers.
data Nat = Z | S Nat
  deriving (Eq, Ord, Show)

type family Succ (n :: Nat) :: Nat where
  Succ 'Z     = 'S 'Z
  Succ ('S n) = n

type family Pred (n :: Nat) :: Nat where
  Pred ('S n) = n

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

class NATTY (n :: Nat) where
  natty :: Natty n
instance NATTY 'Z where
  natty = Zy
instance NATTY n => NATTY ('S n) where
  natty = Sy natty

data OWOTO :: Nat -> Nat -> * where
  LE :: (Compare x y ~ 'LT, LtN x y, LeN x y) => OWOTO x y
  EE :: (Compare x x ~ 'EQ, LeN x x) => OWOTO x x
  GE :: (Compare x y ~ 'GT, LtN y x, LeN y x) => OWOTO x y

owotoNat :: Natty m -> Natty n -> OWOTO m n
owotoNat Zy      Zy      = EE
owotoNat Zy      (Sy _)  = LE
owotoNat (Sy _)  Zy      = GE
owotoNat (Sy m)  (Sy n)  = case owotoNat m n of
  LE -> LE
  GE -> GE
  EE -> EE

-- type family Compare (m :: Nat) (n :: Nat) :: Ordering where
--   Compare 'Z      'Z      = 'EQ
--   Compare ('S m)  ('S n)  = Compare m n
--   Compare ('S m)  'Z      = 'GT
--   Compare 'Z      ('S n)  = 'LT

type family Compare (m :: k) (n :: k') :: Ordering where
  Compare 'Z      'Z      = 'EQ
  Compare ('S m)  ('S n)  = Compare m n
  Compare ('S m)  'Z      = 'GT
  Compare 'Z      ('S n)  = 'LT
  Compare (Natty m) (Natty n) = Compare m n
  Compare (Proxy m) (Proxy n) = Compare m n
  Compare (Proxy m) n = Compare m n
  Compare m (Proxy n) = Compare m n

type family CompareNat (m :: Nat) (n :: Nat) :: Ordering where
  CompareNat 'Z      'Z      = 'EQ
  CompareNat ('S m)  ('S n)  = CompareNat m n
  CompareNat ('S m)  'Z      = 'GT
  CompareNat 'Z      ('S n)  = 'LT

class LeN (n1 :: Nat) (n2 :: Nat) where
instance LeN 'Z      'Z      where
instance LeN 'Z      ('S n2) where
instance LeN n1 n2 =>
  LeN ('S n1) ('S n2) where

class LtN (n1 :: Nat) (n2 :: Nat) where
instance LtN 'Z      ('S n2) where
instance LtN n1 n2 =>
  LtN ('S n1) ('S n2) where
