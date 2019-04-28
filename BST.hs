{-# LANGUAGE DataKinds #-}

{-# LANGUAGE GADTs #-}

{-# LANGUAGE KindSignatures #-}

{-# LANGUAGE PolyKinds #-}

{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE TypeOperators #-}

module BST () where

-- Maybe it's cleaner to separate the Nats that are the keys from the Nats
-- that are bounds.
data Nat = Z | S Nat
  deriving (Eq, Ord, Show)

data BNat :: * where
  Val       :: Nat -> BNat
  MinusInf  :: BNat
  PlusInf   :: BNat
  deriving (Eq, Show)

instance Ord BNat where
  compare MinusInf  MinusInf  = EQ
  compare MinusInf  _         = LT
  compare _         MinusInf  = GT
  compare PlusInf   PlusInf   = EQ
  compare PlusInf   _         = GT
  compare _         PlusInf   = LT
  compare (Val m)   (Val n)   = compare m n   -- use Ord instance of Nat

type family Nat2BNat (n :: Nat) :: BNat
type instance Nat2BNat 'Z     = 'Val 'Z
-- type instance Nat2BNat ('S n) = 'Val ('S m)
--   where
--     'Val m = Nat2BNat n

-- we don't only want to know the MaxnDif but also that they are different
type family MaxnDif (m :: BNat) (n :: BNat) :: BNat
type instance MaxnDif ('Val 'Z)     ('Val ('S n)) = 'Val ('S n)
type instance MaxnDif ('Val ('S m)) ('Val 'Z)     = 'Val ('S m)
type instance MaxnDif ('Val ('S m)) ('Val ('S n)) = MaxnDif ('Val m) ('Val n)
type instance MaxnDif 'MinusInf     ('Val 'Z)     = 'Val 'Z
type instance MaxnDif 'MinusInf     ('Val ('S n)) = 'Val ('S n)
type instance MaxnDif 'MinusInf     'PlusInf      = 'PlusInf
type instance MaxnDif ('Val 'Z)     'MinusInf     = 'Val 'Z
type instance MaxnDif ('Val ('S n)) 'MinusInf     = 'Val ('S n)
type instance MaxnDif 'PlusInf      'MinusInf     = 'PlusInf
type instance MaxnDif 'PlusInf      ('Val 'Z)     = 'PlusInf
type instance MaxnDif 'PlusInf      ('Val ('S n)) = 'PlusInf
type instance MaxnDif ('Val 'Z)     'PlusInf      = 'PlusInf
type instance MaxnDif ('Val ('S _)) 'PlusInf      = 'PlusInf

data SingletonNat :: Nat -> * where
  Zy :: SingletonNat 'Z
  Sy :: SingletonNat n -> SingletonNat ('S n)
deriving instance Eq (SingletonNat a)
deriving instance Show (SingletonNat a)

deSingletonNat :: SingletonNat n -> Nat
deSingletonNat Zy     = Z
deSingletonNat (Sy n) = S (deSingletonNat n)

type family DeSingletonNat n :: Nat
type instance DeSingletonNat (SingletonNat 'Z) = 'Z
type instance DeSingletonNat (SingletonNat ('S n)) =
  'S (DeSingletonNat (SingletonNat n))

data SingletonBNat :: BNat -> * where
  Natty     :: SingletonNat n -> SingletonBNat ('Val n)
  MinusInfy :: SingletonBNat 'MinusInf
  PlusInfy  :: SingletonBNat 'PlusInf
deriving instance Eq (SingletonBNat a)
deriving instance Show (SingletonBNat a)

type family DeSingletonBNat n :: BNat
type instance DeSingletonBNat (SingletonBNat ('Val n)) =
  'Val (DeSingletonNat (SingletonNat n))
type instance DeSingletonBNat (SingletonBNat 'MinusInf) = 'MinusInf
type instance DeSingletonBNat (SingletonBNat 'PlusInf) = 'PlusInf

deSingletonBNat :: SingletonBNat n -> BNat
deSingletonBNat MinusInfy = MinusInf
deSingletonBNat (Natty n) = Val $ deSingletonNat n
deSingletonBNat PlusInfy  = PlusInf

-- Binary Search Tree.
-- TODO only accept Nats in the nodes, but accept BNats as the tree's bounds.
data BST :: BNat -> BNat -> * where
  EmptyBST :: (MaxnDif lb up ~ up) =>
    SingletonBNat lb -> SingletonBNat up -> BST lb up
  RootBST  :: (MaxnDif lb1 n ~ n, MaxnDif n up2 ~ up2) =>
    BST lb1 n -> SingletonBNat n -> BST n up2 -> BST lb1 up2
deriving instance Show (BST l r)

-- Insert a BNat (TODO change to only Nat) into a BST.
insert :: (MaxnDif lb n ~ n, MaxnDif n up ~ up) =>
  SingletonBNat n -> BST lb up -> BST lb up
-- insert :: SingletonBNat n -> BST lb up -> BST lb up
insert n (EmptyBST lb up) = RootBST (EmptyBST lb n) n (EmptyBST n up)
insert n (RootBST l m r) = case compare (deSingletonBNat n) (deSingletonBNat m) of
  -- LT -> RootBST (insert n l) m r
  EQ -> RootBST l m r
  -- GT -> RootBST l m (insert n r)
