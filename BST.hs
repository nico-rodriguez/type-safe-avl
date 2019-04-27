{-# LANGUAGE DataKinds #-}

{-# LANGUAGE GADTs #-}

{-# LANGUAGE KindSignatures #-}

{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE TypeOperators #-}

module BST () where

-- Maybe it's cleaner to separate the Nats that are the keys from the Nats
-- that are bounds.
data Nat = Z | S Nat
  deriving (Eq, Show)

data BNat :: * where
  Val       :: Nat -> BNat
  MinusInf  :: BNat
  PlusInf   :: BNat
  deriving (Eq, Show)

-- we don't only want to know the MaxnDif but also that they are different
type family MaxnDif (m :: BNat) (n :: BNat) :: BNat
type instance MaxnDif ('Val Z)      ('Val (S n))  = 'Val (S n)
type instance MaxnDif ('Val (S m))  ('Val Z)      = 'Val (S m)
type instance MaxnDif ('Val (S m))  ('Val (S n))  = MaxnDif ('Val m) ('Val n)
type instance MaxnDif 'MinusInf     ('Val Z)      = 'Val Z
type instance MaxnDif 'MinusInf     ('Val (S n))  = 'Val (S n)
type instance MaxnDif 'MinusInf     'PlusInf      = 'PlusInf
type instance MaxnDif ('Val Z)      'MinusInf     = 'Val Z
type instance MaxnDif ('Val (S n))  'MinusInf     = 'Val (S n)
type instance MaxnDif 'PlusInf      'MinusInf     = 'PlusInf
type instance MaxnDif 'PlusInf      ('Val Z)      = 'PlusInf
type instance MaxnDif 'PlusInf      ('Val (S n))  = 'PlusInf
type instance MaxnDif ('Val Z)      'PlusInf      = 'PlusInf
type instance MaxnDif ('Val (S _))  'PlusInf      = 'PlusInf

data SingletonNat :: Nat -> * where
  Zy        :: SingletonNat 'Z
  Sy        :: SingletonNat n -> SingletonNat ('S n)
deriving instance Eq (SingletonNat a)
deriving instance Show (SingletonNat a)

data SingletonBNat :: BNat -> * where
  Natty     :: SingletonNat n -> SingletonBNat ('Val (S n))
  MinusInfy :: SingletonBNat 'MinusInf
  PlusInfy  :: SingletonBNat 'PlusInf
deriving instance Eq (SingletonBNat a)
deriving instance Show (SingletonBNat a)

instance Ord (SingletonBNat a) where
  compare MinusInfy       MinusInfy       = EQ
  compare _               MinusInfy       = GT
  compare MinusInfy       _               = LT
  compare PlusInfy        PlusInfy        = EQ
  compare _               PlusInfy        = LT
  compare PlusInfy        _               = GT
  compare (Natty Zy)      (Natty Zy)      = EQ
  compare (Natty (Sy n))  (Natty Zy)      = GT
  compare (Natty Zy)      (Natty (Sy m))  = LT
  compare (Natty (Sy n))  (Natty (Sy m))  = compare n m

-- Binary Search Tree.
-- TODO only accept Nats in the nodes, but accept BNats as the tree's bounds.
data BST :: BNat -> BNat -> * where
  EmptyBST :: (MaxnDif lb up ~ up) =>
    SingletonBNat lb -> SingletonBNat up -> BST lb up
  RootBST  :: (MaxnDif lb1 n ~ n, MaxnDif n up2 ~ up2) =>
    BST lb1 n -> SingletonBNat n -> BST n up2 -> BST lb1 up2

-- Insert a BNat (TODO change to only Nat) into a BST.
insert :: (MaxnDif lb n ~ n, MaxnDif n up ~ up) =>
  SingletonBNat n -> BST lb up -> BST lb up
insert n (EmptyBST lb up) = RootBST (EmptyBST lb n) n (EmptyBST n up)
insert n (RootBST l (Natty Zy) r) = case compare n (Natty Zy) of
  LT -> RootBST (insert n l) (Natty Zy) r
  EQ -> RootBST l (Natty Zy) r
