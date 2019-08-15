{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Import the ITree module and add the constrint of binary search tree to the
-- | types of the trees using type classes.

module ITreeNatIncremental.BST where

import Data.Nat (Nat, Natty, owotoNat, OWOTO (..), Compare)
import ITreeNatIncremental.ITree

-- | Class of Binary Search Trees
class IsBST (t :: Tree) where
instance IsBST 'EmptyTree where
instance (IsBST l, IsBST r, LtN l n, GtN r n) =>
  IsBST ('ForkTree l n r) where

-- | Class of trees whose elements are all strictly less than a given Nat
class LtN (t :: Tree) (x :: Nat) where
instance LtN 'EmptyTree x where
instance (LtN l x, Compare n x ~ 'LT, LtN r x) =>
  LtN ('ForkTree l n r) x

-- | Class of trees whose elements are all strictly greater than a given Nat
class GtN (t :: Tree) (x :: Nat) where
instance GtN 'EmptyTree x where
instance (GtN l x, Compare n x ~ 'GT, GtN r x) =>
  GtN ('ForkTree l n r) x

data BST :: Tree -> * where
  BST :: (IsBST t) => ITree t -> BST t

instance Show (BST t) where
  show (BST t) = "BST $ " ++ show t

-- | Proof object that shows the invariants of Binary Search Tree
data ProofBST :: Tree -> * where
  PE :: ProofBST 'EmptyTree
  PF :: (IsBST l, IsBST r, LtN l n, GtN r n, IsBST ('ForkTree l n r)) =>
    ProofBST l -> Natty n -> ProofBST r -> ProofBST ('ForkTree l n r)

-- | Construct the proof object that the given tree is in fact a Binary Search Tree
proofBST :: ITree t -> ProofBST t
proofBST EmptyITree        = PE
proofBST (ForkITree l n r) = case proofGTN r n of
  PGTN _ _ -> case proofLTN l n of
    PLTN _ _ -> PF (proofBST l) n (proofBST r)

-- | Proof object that all elements of a tree are strictly less than a given Nat
data LTN :: Tree -> Nat -> * where
  PLTN :: (LtN t n, IsBST t) =>
    ITree t -> Natty n -> LTN t n

-- | Construct the proof object that all elements of the given tree
-- | are strictly less than a given Nat
proofLTN :: ITree t -> Natty n -> LTN t n
proofLTN EmptyITree         n = PLTN EmptyITree n
proofLTN t@(ForkITree l n1 r) n = case proofLTN l n of
  PLTN _ _ -> case proofLTN r n of
    PLTN _ _ -> case owotoNat n1 n of
      EE -> undefined -- | Impossible case since we want to prove LtN t n
      LE -> case proofBST t of
        PF{} -> PLTN t n
      GE -> undefined -- | Impossible case since we want to prove LtN t n

-- | Proof object that all elements of a tree are strictly greater than a given Nat
data GTN :: Tree -> Nat -> * where
  PGTN :: (GtN t n, IsBST t) =>
    ITree t -> Natty n -> GTN t n

-- | Construct the proof object that all elements of the given tree
-- | are strictly greater than a given Nat
proofGTN :: ITree t -> Natty n -> GTN t n
proofGTN EmptyITree           n = PGTN EmptyITree n
proofGTN t@(ForkITree l n1 r) n = case proofGTN r n of
  PGTN _ _ -> case proofGTN l n of
    PGTN _ _ -> case owotoNat n1 n of
      EE -> undefined -- | Impossible case since we want to prove GtN t n
      LE -> undefined -- | Impossible case since we want to prove GtN t n
      GE -> case proofBST t of
        PF{} -> PGTN t n

insertBST :: Natty x -> BST t -> BST (Insert x t)
insertBST x (BST t) = let
  t' = insert x t
  in case proofBST t' of
    PE   -> undefined -- | Impossible case since t' = insert x t
    PF{} -> BST t'

deleteBST :: Natty x -> BST t -> BST (Delete x t)
deleteBST x (BST t) = let
  t' = delete x t
  in case proofBST t' of
    PE   -> BST EmptyITree
    PF{} -> BST t'

memberBST :: Natty x -> BST t -> Bool
memberBST x (BST t) = member x t
