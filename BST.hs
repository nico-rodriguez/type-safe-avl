{-# LANGUAGE DataKinds #-}

{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE GADTs #-}

{-# LANGUAGE KindSignatures #-}

{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE PolyKinds #-}

{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE TypeOperators #-}

module BST (Nat(..), Bound(..), BST(..),
  isEmpty, member, insert, delete, preorder, inorder, postorder, outorder) where

import Prelude hiding (max)

-- Natural Numbers.
data Nat = Z | S Nat
  deriving (Eq, Ord, Show)

-- Singleton for Natural Numbers.
data Natty :: Nat -> * where
  Zy :: Natty 'Z
  Sy :: Natty n -> Natty ('S n)
deriving instance Eq (Natty n)
deriving instance Show (Natty n)

natty2Nat :: Natty n -> Nat
natty2Nat Zy      = Z
natty2Nat (Sy n)  = S m
  where
    m = natty2Nat n

-- Type class for ordering Natural Numbers
class LeN (m :: Nat) (n :: Nat) where
instance            LeN 'Z     n      where
instance LeN m n => LeN ('S m) ('S n) where

data OWOTO :: Nat -> Nat -> * where
  LE :: LeN x y => OWOTO x y
  GE :: LeN y x => OWOTO x y
  EE ::            OWOTO x x

owoto :: Natty m -> Natty n -> OWOTO m n
owoto Zy      _       = LE
owoto (Sy _)  Zy      = GE
owoto (Sy m)  (Sy n)  = case owoto m n of
  LE -> LE
  GE -> GE
  EE -> EE

-- Natural Numbers with infinite bounds: Bot is -Inf and Top is +Inf.
data Bound x = Bot | Val x | Top
  deriving (Eq, Ord, Show)

-- Type class for ordering Bounded Natural Numbers.
class LeB (m :: Bound Nat) (n :: Bound Nat) where
instance            LeB 'Bot      ('Val y)  where
instance            LeB 'Bot      'Top      where
instance LeN x y => LeB ('Val x)  ('Val y)  where
instance            LeB ('Val x)  'Top      where

-- Binary Search Tree of Singleton Natural Numbers.
data BST :: Bound Nat -> Bound Nat -> * where
  EmptyBST :: (LeB lb up) => BST lb up
  RootBST  :: (LeB lb ('Val n), LeB ('Val n) up) =>
    BST lb ('Val n) -> Natty n -> BST ('Val n) up -> BST lb up
deriving instance Show (BST l r)

isEmpty :: BST lb up -> Bool
isEmpty EmptyBST = True
isEmpty _        = False

member :: BST lb up -> Natty n -> Bool
member EmptyBST         _ = False
member (RootBST l m r)  x = case owoto m x of
  LE -> member r x
  EE -> True
  GE -> member l x

-- Insert a Singleton Nat (Natty) into a BST.
insert :: (LeB lb ('Val n), LeB ('Val n) up) => Natty n -> BST lb up -> BST lb up
insert n EmptyBST = RootBST EmptyBST n EmptyBST
insert n (RootBST l m r) = case owoto n m of
  LE -> RootBST (insert n l) m r
  EE -> RootBST l m r
  GE -> RootBST l m (insert n r)

-- TODO Check the non-emptyness of the tree through it's type
max :: BST lb up -> Natty n
max (RootBST _ m r) = if isEmpty r
                      then m
                      else max r

delete :: (LeB lb (Val n), LeB (Val n) up, LeB lb' up') =>
  Natty n -> BST lb up -> BST lb' up'
delete _ EmptyBST = EmptyBST
delete n (RootBST l m r) = case owoto n m of
  LE -> RootBST (delete n l) m r
  EE -> case isEmpty l of
          True  -> r
          False -> RootBST l1 m1 r
            where
              m1 = max l
              l1 = delete m1 l
  GE -> RootBST l m (delete n r)

preorder :: BST lb up -> [Nat]
preorder EmptyBST         = []
preorder (RootBST l m r)  = (natty2Nat m : preorder l) ++ preorder r

inorder :: BST lb up -> [Nat]
inorder EmptyBST        = []
inorder (RootBST l m r) = inorder l ++ [natty2Nat m] ++ inorder r

outorder :: BST lb up -> [Nat]
outorder EmptyBST        = []
outorder (RootBST l m r) = outorder r ++ [natty2Nat m] ++ outorder l

postorder :: BST lb up -> [Nat]
postorder EmptyBST        = []
postorder (RootBST l m r) = postorder l ++ postorder r ++ [natty2Nat m]
