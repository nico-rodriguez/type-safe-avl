{-# LANGUAGE DataKinds #-}

{-# LANGUAGE GADTs #-}

{-# LANGUAGE KindSignatures #-}

{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE TypeFamilies #-}

module Nat where

-- Natural Numbers.
data Nat = Z | S Nat
  deriving (Eq, Ord, Show)

-- Singleton for Natural Numbers.
data Natty :: Nat -> * where
  Zy :: Natty 'Z
  Sy :: Natty n -> Natty ('S n)
deriving instance Eq (Natty n)
deriving instance Show (Natty n)

data OWOTO :: Nat -> Nat -> * where
  LE :: (Compare x y ~ 'LT)  => OWOTO x y
  EE :: (Compare x x ~ 'EQ)           => OWOTO x x
  GE :: (Compare x y ~ 'GT)  => OWOTO x y

owoto :: Natty m -> Natty n -> OWOTO m n
owoto Zy      Zy      = EE
owoto Zy      (Sy _)  = LE
owoto (Sy _)  Zy      = GE
owoto (Sy m)  (Sy n)  = case owoto m n of
  LE -> LE
  GE -> GE
  EE -> EE

type family Compare (m :: Nat) (n :: Nat) :: Ordering where
  Compare 'Z      'Z      = 'EQ
  Compare ('S m)  ('S n)  = Compare m n
  Compare ('S m)  'Z      = 'GT
  Compare 'Z      ('S n)  = 'LT
