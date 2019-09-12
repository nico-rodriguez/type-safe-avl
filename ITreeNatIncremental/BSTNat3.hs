{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ITreeNatIncremental.BSTNat3 where

import Data.Proxy
import Data.Type.Bool
import Data.Type.Equality
import Data.Nat (Nat, Compare, Natty)

data Tree :: * where
  EmptyTree :: Tree
  ForkTree  :: Tree -> Nat -> Tree -> Tree

-- | Check if all elements of the tree are strictly less than x
type family LtN (l :: Tree) (x :: Nat) :: Bool where
  LtN 'EmptyTree        x = 'True
  LtN ('ForkTree l n r) x =
    (If (Compare n x == 'LT && LtN l x == 'True && LtN r x == 'True)
      'True
      'False
    )

-- | Check if all elements of the tree are strictly greater than x
type family GtN (r :: Tree) (x :: Nat) :: Bool where
  GtN 'EmptyTree        x = 'True
  GtN ('ForkTree l n r) x =
    (If (Compare n x == 'GT && GtN l x == 'True && GtN r x == 'True)
      'True
      'False
    )

type family IsBST (t :: Tree) :: Bool where
  IsBST 'EmptyTree = 'True
  IsBST ('ForkTree l n r) = IsBST l && IsBST r && LtN l n && GtN r n

data ITree :: Tree -> * where
  EmptyITree :: ITree 'EmptyTree
  ForkITree  :: ITree l -> Natty n -> ITree r -> ITree ('ForkTree l n r)

instance Show (ITree t) where
  show EmptyITree         = "E"
  show (ForkITree l n r)  = "F " ++ go l ++ " " ++ show n ++ " " ++ go r
    where
      go :: ITree t' -> String
      go EmptyITree         = "E"
      go (ForkITree l' n' r')  = "(F " ++ go l' ++ " " ++ show n' ++ " " ++ go r' ++ ")"

data BST :: Tree -> * where
  BST :: (IsBST t ~ 'True) => ITree t -> BST t

instance Show (BST t) where
  show (BST t) = "BST $ " ++ show t

class Insertable (x :: Nat) (t :: Tree) where
  type Insert (x :: Nat) (t :: Tree) :: Tree
  insert :: Natty x -> ITree t -> ITree (Insert x t)
instance Insertable x 'EmptyTree where
  type Insert x 'EmptyTree = 'ForkTree 'EmptyTree x 'EmptyTree
  insert x EmptyITree         = ForkITree EmptyITree x EmptyITree
instance Insertable' x ('ForkTree l n r) (Compare x n) => Insertable x ('ForkTree l n r) where
  type Insert x ('ForkTree l n r) = Insert' x ('ForkTree l n r) (Compare x n)
  insert x t = insert' x t (Proxy::Proxy (Compare x n))

class Insertable' (x :: Nat) (t :: Tree) (o :: Ordering) where
  type Insert' (x :: Nat) (t :: Tree) (o :: Ordering) :: Tree
  insert' :: Natty x -> ITree t -> Proxy o -> ITree (Insert x t)
instance Compare x n ~ 'EQ => Insertable' x ('ForkTree l n r) 'EQ where
  type Insert' x ('ForkTree l n r) 'EQ = 'ForkTree l n r
  insert' _ t _ = t
instance Compare x n ~ 'LT => Insertable' x ('ForkTree 'EmptyTree n r) 'LT where
  type Insert' x ('ForkTree 'EmptyTree n r) 'LT = 'ForkTree ('ForkTree 'EmptyTree x 'EmptyTree) n r
  insert' x (ForkITree EmptyITree n r) _ = ForkITree (ForkITree EmptyITree x EmptyITree) n r
instance (Compare x n ~ 'LT, l ~ 'ForkTree ll ln lr, Insertable' x l (Compare x ln)) => Insertable' x ('ForkTree ('ForkTree ll ln lr) n r) 'LT where
  type Insert' x ('ForkTree ('ForkTree ll ln lr) n r) 'LT = 'ForkTree (Insert' x ('ForkTree ll ln lr) (Compare x ln)) n r
  insert' x (ForkITree l@ForkITree{} n r) _ = ForkITree (insert' x l (Proxy::Proxy (Compare x ln))) n r
instance Compare x n ~ 'GT => Insertable' x ('ForkTree l n 'EmptyTree) 'GT where
  type Insert' x ('ForkTree l n 'EmptyTree) 'GT = 'ForkTree l n ('ForkTree 'EmptyTree x 'EmptyTree)
  insert' x (ForkITree l n EmptyITree) _ = ForkITree l n (ForkITree EmptyITree x EmptyITree)
instance (Compare x n ~ 'GT, r ~ 'ForkTree rl rn rr, Insertable' x r (Compare x rn)) => Insertable' x ('ForkTree l n ('ForkTree rl rn rr)) 'GT where
  type Insert' x ('ForkTree l n ('ForkTree rl rn rr)) 'GT = 'ForkTree l n (Insert' x ('ForkTree rl rn rr) (Compare x rn))
  insert' x (ForkITree l n r@ForkITree{}) _ = ForkITree l n (insert' x r (Proxy::Proxy (Compare x rn)))

class ProofIsBSTInsert (x :: Nat) (t :: Tree) where
  proofIsBSTInsert :: (IsBST t ~ 'True) =>
    Natty x -> ITree t -> IsBST (Insert x t) :~: 'True
instance ProofIsBSTInsert x 'EmptyTree where
  proofIsBSTInsert _ EmptyITree = Refl
instance ProofIsBSTInsert' x ('ForkTree l n r) (Compare x n) => ProofIsBSTInsert x ('ForkTree l n r) where
  proofIsBSTInsert x t = proofIsBSTInsert' x t (Proxy::Proxy (Compare x n))

class ProofIsBSTInsert' (x :: Nat) (t :: Tree) (o :: Ordering) where
  proofIsBSTInsert' :: (t ~ 'ForkTree l n r) => Natty x -> ITree t -> Proxy o -> IsBST (Insert' x t o) :~: 'True
instance (Compare x n ~ 'EQ, IsBST l ~ 'True, IsBST r ~ 'True, LtN l n ~ 'True, GtN r n ~ 'True) =>
  ProofIsBSTInsert' x ('ForkTree l n r) 'EQ where
  proofIsBSTInsert' _ ForkITree{} _ = Refl
instance (l ~ 'EmptyTree, Compare x n ~ 'LT, IsBST l ~ 'True, IsBST r ~ 'True, LtN l n ~ 'True, GtN r n ~ 'True) =>
  ProofIsBSTInsert' x ('ForkTree 'EmptyTree n r) 'LT where
  proofIsBSTInsert' _ (ForkITree EmptyITree _ _) _ = Refl
instance (l ~ 'ForkTree ll ln lr, Compare x n ~ 'LT, IsBST l ~ 'True, IsBST r ~ 'True, LtN l n ~ 'True, GtN r n ~ 'True, ProofIsBSTInsert' x l (Compare x ln),
  ProofLtNInsert' x l n (Compare x ln)) =>
  ProofIsBSTInsert' x ('ForkTree ('ForkTree ll ln lr) n r) 'LT where
  proofIsBSTInsert' x (ForkITree l@ForkITree{} n _) _ = gcastWith (proofLtNInsert' x l n (Proxy::Proxy (Compare x ln))) (gcastWith (proofIsBSTInsert' x l (Proxy::Proxy (Compare x ln))) Refl)
instance (r ~ 'EmptyTree, Compare x n ~ 'GT, IsBST l ~ 'True, IsBST r ~ 'True, LtN l n ~ 'True, GtN r n ~ 'True) =>
  ProofIsBSTInsert' x ('ForkTree l n 'EmptyTree) 'GT where
  proofIsBSTInsert' _ (ForkITree _ _ EmptyITree) _ = Refl
instance (r ~ 'ForkTree rl rn rr, Compare x n ~ 'GT, IsBST r ~ 'True, IsBST l ~ 'True, GtN r n ~ 'True, LtN l n ~ 'True, ProofIsBSTInsert' x r (Compare x rn),
  ProofGtNInsert' x r n (Compare x rn)) =>
  ProofIsBSTInsert' x ('ForkTree l n ('ForkTree rl rn rr)) 'GT where
  proofIsBSTInsert' x (ForkITree _ n r@ForkITree{}) _ = gcastWith (proofGtNInsert' x r n (Proxy::Proxy (Compare x rn))) (gcastWith (proofIsBSTInsert' x r (Proxy::Proxy (Compare x rn))) Refl)

class ProofLtNInsert' (x :: Nat) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofLtNInsert' :: (t ~ 'ForkTree l n1 r, Compare x n ~ 'LT, LtN t n ~ 'True) =>
    Natty x -> ITree t -> Natty n -> Proxy o -> LtN (Insert x t) n :~: 'True
instance (t ~ 'ForkTree l n1 r, Compare x n1 ~ 'EQ, Compare x n ~ 'LT, Compare n1 n ~ 'LT, LtN l n ~ 'True, LtN r n ~ 'True) =>
  ProofLtNInsert' x ('ForkTree l n1 r) n 'EQ where
  proofLtNInsert' _ ForkITree{} _ _ = Refl
instance (t ~ 'ForkTree l n1 r, l ~ 'EmptyTree, Compare x n1 ~ 'LT, Compare x n ~ 'LT, Compare n1 n ~ 'LT, LtN l n ~ 'True, LtN r n ~ 'True) =>
  ProofLtNInsert' x ('ForkTree 'EmptyTree n1 r) n 'LT where
  proofLtNInsert' _ (ForkITree EmptyITree _ _) _ _ = Refl
instance (t ~ 'ForkTree l n1 r, l ~ 'ForkTree ll ln lr, Compare x n1 ~ 'LT, Compare x n ~ 'LT, Compare n1 n ~ 'LT, LtN l n ~ 'True, LtN r n ~ 'True, ProofLtNInsert' x l n (Compare x ln)) =>
  ProofLtNInsert' x ('ForkTree ('ForkTree ll ln lr) n1 r) n 'LT where
  proofLtNInsert' x (ForkITree l@ForkITree{} _ _) n _ = gcastWith (proofLtNInsert' x l n (Proxy::Proxy (Compare x ln))) Refl
instance (t ~ 'ForkTree l n1 r, r ~ 'EmptyTree, Compare x n1 ~ 'GT, Compare x n ~ 'LT, Compare n1 n ~ 'LT, LtN l n ~ 'True, LtN r n ~ 'True) =>
  ProofLtNInsert' x ('ForkTree l n1 'EmptyTree) n 'GT where
  proofLtNInsert' _ (ForkITree _ _ EmptyITree) _ _ = Refl
instance (t ~ 'ForkTree l n1 r, r ~ 'ForkTree rl rn rr, Compare x n1 ~ 'GT, Compare x n ~ 'LT, Compare n1 n ~ 'LT, LtN l n ~ 'True, LtN r n ~ 'True, ProofLtNInsert' x r n (Compare x rn)) =>
  ProofLtNInsert' x ('ForkTree l n1 ('ForkTree rl rn rr)) n 'GT where
  proofLtNInsert' x (ForkITree _ _ r@ForkITree{}) n _ = gcastWith (proofLtNInsert' x r n (Proxy::Proxy (Compare x rn))) Refl

class ProofGtNInsert' (x :: Nat) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofGtNInsert' :: (t ~ 'ForkTree l n1 r, Compare x n ~ 'GT, GtN t n ~ 'True) =>
    Natty x -> ITree t -> Natty n -> Proxy o -> GtN (Insert x t) n :~: 'True
instance (t ~ 'ForkTree l n1 r, Compare x n1 ~ 'EQ, Compare x n ~ 'GT, Compare n1 n ~ 'GT, GtN l n ~ 'True, GtN r n ~ 'True) =>
  ProofGtNInsert' x ('ForkTree l n1 r) n 'EQ where
  proofGtNInsert' _ ForkITree{} _ _ = Refl
instance (t ~ 'ForkTree l n1 r, l ~ 'EmptyTree, Compare x n1 ~ 'LT, Compare x n ~ 'GT, Compare n1 n ~ 'GT, GtN l n ~ 'True, GtN r n ~ 'True) =>
  ProofGtNInsert' x ('ForkTree 'EmptyTree n1 r) n 'LT where
  proofGtNInsert' _ (ForkITree EmptyITree _ _) _ _ = Refl
instance (t ~ 'ForkTree l n1 r, l ~ 'ForkTree ll ln lr, Compare x n1 ~ 'LT, Compare x n ~ 'GT, Compare n1 n ~ 'GT, GtN l n ~ 'True, GtN r n ~ 'True, ProofGtNInsert' x l n (Compare x ln)) =>
  ProofGtNInsert' x ('ForkTree ('ForkTree ll ln lr) n1 r) n 'LT where
  proofGtNInsert' x (ForkITree l@ForkITree{} _ _) n _ = gcastWith (proofGtNInsert' x l n (Proxy::Proxy (Compare x ln))) Refl
instance (t ~ 'ForkTree l n1 r, r ~ 'EmptyTree, Compare x n1 ~ 'GT, Compare x n ~ 'GT, Compare n1 n ~ 'GT, GtN l n ~ 'True, GtN r n ~ 'True) =>
  ProofGtNInsert' x ('ForkTree l n1 'EmptyTree) n 'GT where
  proofGtNInsert' _ (ForkITree _ _ EmptyITree) _ _ = Refl
instance (t ~ 'ForkTree l n1 r, r ~ 'ForkTree rl rn rr, Compare x n1 ~ 'GT, Compare x n ~ 'GT, Compare n1 n ~ 'GT, GtN l n ~ 'True, GtN r n ~ 'True, ProofGtNInsert' x r n (Compare x rn)) =>
  ProofGtNInsert' x ('ForkTree l n1 ('ForkTree rl rn rr)) n 'GT where
  proofGtNInsert' x (ForkITree _ _ r@ForkITree{}) n _ = gcastWith (proofGtNInsert' x r n (Proxy::Proxy (Compare x rn))) Refl

insertBST :: (Insertable x t, ProofIsBSTInsert x t) =>
  Natty x -> BST t -> BST (Insert x t)
insertBST x (BST t) = gcastWith (proofIsBSTInsert x t) BST $ insert x t
