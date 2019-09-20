{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module ITreeNatIncremental.BST where

import           Data.Kind
import           Data.Nat           (Compare, NATTY (..), Nat (..), Natty)
import           Data.Proxy
import           Data.Type.Bool
import           Data.Type.Equality
import           ITreeNatIncremental.Node
import           ITreeNatIncremental.ITree
import           Prelude            hiding (lookup)

type family IsBST (t :: Tree) :: Bool where
  IsBST 'EmptyTree = 'True
  IsBST ('ForkTree l (Node n a) r) = IsBST l && IsBST r && LtN l n && GtN r n

data BST :: Tree -> Type where
  BST :: (IsBST t ~ 'True) => ITree t -> BST t

instance Show (BST t) where
  show (BST t) = "BST $ " ++ show t

class ProofIsBSTInsert (x :: Nat) (a :: Type) (t :: Tree) where
  proofIsBSTInsert :: (IsBST t ~ 'True) =>
    Node x a -> ITree t -> IsBST (Insert x a t) :~: 'True
instance ProofIsBSTInsert x a 'EmptyTree where
  proofIsBSTInsert _ EmptyITree = Refl
instance ProofIsBSTInsert' x a ('ForkTree l (Node n a1) r) (Compare x n) => ProofIsBSTInsert x a ('ForkTree l (Node n a1) r) where
  proofIsBSTInsert node t = proofIsBSTInsert' node t (Proxy::Proxy (Compare x n))

class ProofIsBSTInsert' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) where
  proofIsBSTInsert' :: (t ~ 'ForkTree l (Node n a1) r) => Node x a -> ITree t -> Proxy o -> IsBST (Insert' x a t o) :~: 'True
instance (Compare x n ~ 'EQ, IsBST l ~ 'True, IsBST r ~ 'True, LtN l n ~ 'True, GtN r n ~ 'True) =>
  ProofIsBSTInsert' x a ('ForkTree l (Node n a1) r) 'EQ where
  proofIsBSTInsert' _ ForkITree{} _ = Refl
instance (l ~ 'EmptyTree, Compare x n ~ 'LT, IsBST l ~ 'True, IsBST r ~ 'True, LtN l n ~ 'True, GtN r n ~ 'True) =>
  ProofIsBSTInsert' x a ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  proofIsBSTInsert' _ (ForkITree EmptyITree _ _) _ = Refl
instance (NATTY n, l ~ 'ForkTree ll (Node ln lna) lr, Compare x n ~ 'LT, IsBST l ~ 'True, IsBST r ~ 'True, LtN l n ~ 'True, GtN r n ~ 'True, ProofIsBSTInsert' x a l (Compare x ln),
  ProofLtNInsert' x a l n (Compare x ln)) =>
  ProofIsBSTInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT where
  proofIsBSTInsert' node (ForkITree l@ForkITree{} _ _) _ = gcastWith (proofLtNInsert' node l (natty::Natty n) (Proxy::Proxy (Compare x ln))) (gcastWith (proofIsBSTInsert' node l (Proxy::Proxy (Compare x ln))) Refl)
instance (r ~ 'EmptyTree, Compare x n ~ 'GT, IsBST l ~ 'True, IsBST r ~ 'True, LtN l n ~ 'True, GtN r n ~ 'True) =>
  ProofIsBSTInsert' x a ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  proofIsBSTInsert' _ (ForkITree _ _ EmptyITree) _ = Refl
instance (NATTY n, r ~ 'ForkTree rl (Node rn rna) rr, Compare x n ~ 'GT, IsBST r ~ 'True, IsBST l ~ 'True, GtN r n ~ 'True, LtN l n ~ 'True, ProofIsBSTInsert' x a r (Compare x rn),
  ProofGtNInsert' x a r n (Compare x rn)) =>
  ProofIsBSTInsert' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT where
  proofIsBSTInsert' node (ForkITree _ _ r@ForkITree{}) _ = gcastWith (proofGtNInsert' node r (natty::Natty n) (Proxy::Proxy (Compare x rn))) (gcastWith (proofIsBSTInsert' node r (Proxy::Proxy (Compare x rn))) Refl)

class ProofLtNInsert' (x :: Nat) (a :: Type) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofLtNInsert' :: (t ~ 'ForkTree l n1 r, Compare x n ~ 'LT, LtN t n ~ 'True) =>
    Node x a -> ITree t -> Natty n -> Proxy o -> LtN (Insert x a t) n :~: 'True
instance (t ~ 'ForkTree l n1 r, Compare x n1 ~ 'EQ, Compare x n ~ 'LT, Compare n1 n ~ 'LT, LtN l n ~ 'True, LtN r n ~ 'True) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) r) n 'EQ where
  proofLtNInsert' _ ForkITree{} _ _ = Refl
instance (t ~ 'ForkTree l n1 r, l ~ 'EmptyTree, Compare x n1 ~ 'LT, Compare x n ~ 'LT, Compare n1 n ~ 'LT, LtN l n ~ 'True, LtN r n ~ 'True) =>
  ProofLtNInsert' x a ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofLtNInsert' _ (ForkITree EmptyITree _ _) _ _ = Refl
instance (t ~ 'ForkTree l (Node n1 a1) r, l ~ 'ForkTree ll (Node ln lna) lr, Compare x n1 ~ 'LT, Compare x n ~ 'LT, Compare n1 n ~ 'LT, LtN l n ~ 'True, LtN r n ~ 'True,
  ProofLtNInsert' x a l n (Compare x ln)) =>
  ProofLtNInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n1 a1) r) n 'LT where
  proofLtNInsert' node (ForkITree l@ForkITree{} _ _) n _ = gcastWith (proofLtNInsert' node l n (Proxy::Proxy (Compare x ln))) Refl
instance (t ~ 'ForkTree l (Node n1 a1) r, r ~ 'EmptyTree, Compare x n1 ~ 'GT, Compare x n ~ 'LT, Compare n1 n ~ 'LT, LtN l n ~ 'True, LtN r n ~ 'True) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofLtNInsert' _ (ForkITree _ _ EmptyITree) _ _ = Refl
instance (t ~ 'ForkTree l (Node n1 a1) r, r ~ 'ForkTree rl (Node rn rna) rr, Compare x n1 ~ 'GT, Compare x n ~ 'LT, Compare n1 n ~ 'LT, LtN l n ~ 'True, LtN r n ~ 'True,
  ProofLtNInsert' x a r n (Compare x rn)) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn rna) rr)) n 'GT where
  proofLtNInsert' node (ForkITree _ _ r@ForkITree{}) n _ = gcastWith (proofLtNInsert' node r n (Proxy::Proxy (Compare x rn))) Refl

class ProofGtNInsert' (x :: Nat) (a :: Type) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofGtNInsert' :: (t ~ 'ForkTree l n1 r, Compare x n ~ 'GT, GtN t n ~ 'True) =>
    Node x a -> ITree t -> Natty n -> Proxy o -> GtN (Insert x a t) n :~: 'True
instance (t ~ 'ForkTree l (Node n1 a1) r, Compare x n1 ~ 'EQ, Compare x n ~ 'GT, Compare n1 n ~ 'GT, GtN l n ~ 'True, GtN r n ~ 'True) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) r) n 'EQ where
  proofGtNInsert' _ ForkITree{} _ _ = Refl
instance (t ~ 'ForkTree l (Node n1 a1) r, l ~ 'EmptyTree, Compare x n1 ~ 'LT, Compare x n ~ 'GT, Compare n1 n ~ 'GT, GtN l n ~ 'True, GtN r n ~ 'True) =>
  ProofGtNInsert' x a ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofGtNInsert' _ (ForkITree EmptyITree _ _) _ _ = Refl
instance (t ~ 'ForkTree l (Node n1 a1) r, l ~ 'ForkTree ll (Node ln lna) lr, Compare x n1 ~ 'LT, Compare x n ~ 'GT, Compare n1 n ~ 'GT, GtN l n ~ 'True, GtN r n ~ 'True,
  ProofGtNInsert' x a l n (Compare x ln)) =>
  ProofGtNInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n1 a1) r) n 'LT where
  proofGtNInsert' x (ForkITree l@ForkITree{} _ _) n _ = gcastWith (proofGtNInsert' x l n (Proxy::Proxy (Compare x ln))) Refl
instance (t ~ 'ForkTree l (Node n1 a1) r, r ~ 'EmptyTree, Compare x n1 ~ 'GT, Compare x n ~ 'GT, Compare n1 n ~ 'GT, GtN l n ~ 'True, GtN r n ~ 'True) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofGtNInsert' _ (ForkITree _ _ EmptyITree) _ _ = Refl
instance (t ~ 'ForkTree l (Node n1 a1) r, r ~ 'ForkTree rl (Node rn rna) rr, Compare x n1 ~ 'GT, Compare x n ~ 'GT, Compare n1 n ~ 'GT, GtN l n ~ 'True, GtN r n ~ 'True,
  ProofGtNInsert' x a r n (Compare x rn)) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn rna) rr)) n 'GT where
  proofGtNInsert' x (ForkITree _ _ r@ForkITree{}) n _ = gcastWith (proofGtNInsert' x r n (Proxy::Proxy (Compare x rn))) Refl

insertBST :: (Insertable x a t, ProofIsBSTInsert x a t) =>
  Node x a -> BST t -> BST (Insert x a t)
insertBST x (BST t) = gcastWith (proofIsBSTInsert x t) BST $ insert x t

lookupBST :: (t ~ 'ForkTree l (Node n a1) r, Member x t ~ 'True, Lookupable x a t) =>
  Proxy x -> BST t -> a
lookupBST p (BST t) = lookup p t
