{-# LANGUAGE DataKinds #-}

{-# LANGUAGE GADTs #-}

{-# LANGUAGE KindSignatures #-}

{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE TypeOperators #-}

module BST () where

data Nat = Z | S Nat | MinusInf | PlusInf
  deriving (Eq, Show)

-- we don't only want to know the MaxnDif but also that they are different
type family MaxnDif (m :: Nat) (n :: Nat) :: Nat
type instance MaxnDif 'Z        ('S n)    = 'S n
type instance MaxnDif ('S m)    'Z        = 'S m
type instance MaxnDif ('S m)    ('S n)    = 'S (MaxnDif m n)
type instance MaxnDif 'MinusInf 'Z        = 'Z
type instance MaxnDif 'MinusInf ('S n)    = 'S n
type instance MaxnDif 'MinusInf 'PlusInf  = 'PlusInf
type instance MaxnDif 'Z        'MinusInf = 'Z
type instance MaxnDif ('S n)    'MinusInf = 'S n
type instance MaxnDif 'PlusInf  'MinusInf = 'PlusInf
type instance MaxnDif 'PlusInf  'Z        = 'PlusInf
type instance MaxnDif 'PlusInf  ('S n)    = 'PlusInf
type instance MaxnDif 'Z        'PlusInf  = 'PlusInf
type instance MaxnDif ('S _)    'PlusInf  = 'PlusInf

data SingletonNat :: Nat -> * where
  Zy        :: SingletonNat 'Z
  Sy        :: SingletonNat n -> SingletonNat ('S n)
  MinusInfy :: SingletonNat 'MinusInf
  PlusInfy  :: SingletonNat 'PlusInf
deriving instance Eq (SingletonNat a)
deriving instance Show (SingletonNat a)
-- deriving instance Ord (SingletonNat a)

-- class Ord (SingletonNat a) where
--   compare Zy Zy = EQ
--   ...

-- Binary Search Tree.
data BST :: Nat -> Nat -> * where
  EmptyBST :: (MaxnDif lb up ~ up) =>
    SingletonNat lb -> SingletonNat up -> BST lb up
  RootBST  :: (MaxnDif lb1 n ~ n, MaxnDif n up2 ~ up2) =>
    BST lb1 n -> SingletonNat n -> BST n up2 -> BST lb1 up2

insert :: (MaxnDif lb n ~ n, MaxnDif n up ~ up) =>
  SingletonNat n -> BST lb up -> BST lb up
insert n (EmptyBST lb up) = RootBST (EmptyBST lb n) n (EmptyBST n up)
-- insert n (RootBST (BST lb m) m (BST m up)) =
