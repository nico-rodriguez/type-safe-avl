{-# LANGUAGE DataKinds #-}

{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE GADTs #-}

{-# LANGUAGE KindSignatures #-}

{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE PolyKinds #-}

{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE TypeOperators #-}

module BST (Nat(..), SingletonNat(..), BNat(..), SingletonBNat(..),
  BST(..), insert) where

import Prelude hiding (max)

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

class LeN (m :: Nat) (n :: Nat) where
instance            LeN 'Z     n      where
instance LeN m n => LeN ('S m) ('S n) where

-- we don't only want to know the MaxnDif but also that they are different
class LeB (m :: BNat) (n :: BNat) where
instance            LeB 'MinusInf ('Val y) where
instance            LeB 'MinusInf 'PlusInf where
instance LeN x y => LeB ('Val x)  ('Val y) where
instance            LeB ('Val x)  'PlusInf where

data SingletonNat :: Nat -> * where
  Zy :: SingletonNat 'Z
  Sy :: SingletonNat n -> SingletonNat ('S n)
deriving instance Eq (SingletonNat a)
deriving instance Show (SingletonNat a)

deSingletonNat :: SingletonNat n -> Nat
deSingletonNat Zy     = Z
deSingletonNat (Sy n) = S (deSingletonNat n)

data SingletonBNat :: BNat -> * where
  Natty     :: SingletonNat n -> SingletonBNat ('Val n)
  MinusInfy :: SingletonBNat 'MinusInf
  PlusInfy  :: SingletonBNat 'PlusInf
deriving instance Eq (SingletonBNat a)
deriving instance Show (SingletonBNat a)

deSingletonBNat :: SingletonBNat n -> BNat
deSingletonBNat MinusInfy = MinusInf
deSingletonBNat (Natty n) = Val $ deSingletonNat n
deSingletonBNat PlusInfy  = PlusInf

-- Binary Search Tree.
-- TODO only accept Nats in the nodes, but accept BNats as the tree's bounds.
data BST :: BNat -> BNat -> * where
  EmptyBST :: (LeB lb up) =>
    SingletonBNat lb -> SingletonBNat up -> BST lb up
  RootBST  :: (LeB lb n, LeB n up) =>
    BST lb n -> SingletonBNat n -> BST n up -> BST lb up
deriving instance Show (BST l r)

isEmpety :: BST lb up -> Bool
isEmpety (EmptyBST _ _) = True
isEmpety _              = False

member :: BST lb up -> BNat -> Bool
member (EmptyBST _ _)   _ = False
member (RootBST l m r)  x = case compare (deSingletonBNat m) x of
  LT -> member r x
  EQ -> True
  GT -> member l x

-- Insert a BNat (TODO change to only Nat) into a BST.
insert :: (LeB lb n, LeB n up) =>
  SingletonBNat n -> BST lb up -> BST lb up
insert n (EmptyBST lb up) = RootBST (EmptyBST lb n) n (EmptyBST n up)
insert n (RootBST l m r) = case compare (deSingletonBNat n) (deSingletonBNat m) of
  LT -> RootBST (insert n l) m r
  EQ -> RootBST l m r
  -- GT -> RootBST l m (insert n r)

-- TODO Check the non-emptyness of the tree through it's type
max :: BST lb up -> BNat
max (RootBST _ m r) = if isEmpety r
                      then deSingletonBNat m
                      else max r

delete :: (LeB lb n, LeB n up) =>
  SingletonBNat n -> BST lb up -> BST lb up
delete _ (EmptyBST lb up) = EmptyBST lb up
-- delete n (RootBST l m r) = case compare (deSingletonBNat n) (deSingletonBNat m) of
  -- LT -> RootBST (delete n l) m r
  -- EQ -> RootBST l1 m1 r
  --         where
  --           m1 = max l
  --           l1 = delete m1 l
  -- GT -> RootBST l m (delete n r)

preorder :: BST lb up -> [BNat]
preorder (EmptyBST _ _)  = []
preorder (RootBST l m r) = (deSingletonBNat m : inorder l) ++ inorder r

inorder :: BST lb up -> [BNat]
inorder (EmptyBST _ _)  = []
inorder (RootBST l m r) = inorder l ++ [deSingletonBNat m] ++ inorder r

outorder :: BST lb up -> [BNat]
outorder (EmptyBST _ _)  = []
outorder (RootBST l m r) = inorder r ++ [deSingletonBNat m] ++ inorder l

postorder :: BST lb up -> [BNat]
postorder (EmptyBST _ _)  = []
postorder (RootBST l m r) = inorder l ++ inorder r ++ [deSingletonBNat m]
