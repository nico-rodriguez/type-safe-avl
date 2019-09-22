{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module ITreeNatIncremental.BST where

import           Data.Kind
import           Data.Proxy
import           Data.Type.Bool
import           Data.Type.Equality
import           GHC.TypeLits
import           ITreeNatIncremental.ITree
import           ITreeNatIncremental.Node
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
instance ProofIsBSTInsert' x a ('ForkTree l (Node n a1) r) (CmpNat x n) => ProofIsBSTInsert x a ('ForkTree l (Node n a1) r) where
  proofIsBSTInsert node t = proofIsBSTInsert' node t (Proxy::Proxy (CmpNat x n))

class ProofIsBSTInsert' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) where
  proofIsBSTInsert' :: (t ~ 'ForkTree l (Node n a1) r) => Node x a -> ITree t -> Proxy o -> IsBST (Insert' x a t o) :~: 'True
instance (CmpNat x n ~ 'EQ, IsBST l ~ 'True, IsBST r ~ 'True, LtN l n ~ 'True, GtN r n ~ 'True) =>
  ProofIsBSTInsert' x a ('ForkTree l (Node n a1) r) 'EQ where
  proofIsBSTInsert' _ ForkITree{} _ = Refl
instance (l ~ 'EmptyTree, CmpNat x n ~ 'LT, IsBST l ~ 'True, IsBST r ~ 'True, LtN l n ~ 'True, GtN r n ~ 'True) =>
  ProofIsBSTInsert' x a ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  proofIsBSTInsert' _ (ForkITree EmptyITree _ _) _ = Refl
instance (l ~ 'ForkTree ll (Node ln lna) lr, CmpNat x n ~ 'LT, IsBST l ~ 'True, IsBST r ~ 'True, LtN l n ~ 'True, GtN r n ~ 'True, ProofIsBSTInsert' x a l (CmpNat x ln),
  ProofLtNInsert' x a l n (CmpNat x ln)) =>
  ProofIsBSTInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT where
  proofIsBSTInsert' node (ForkITree l@ForkITree{} _ _) _ = gcastWith (proofLtNInsert' node l (Proxy::Proxy n) (Proxy::Proxy (CmpNat x ln))) (gcastWith (proofIsBSTInsert' node l (Proxy::Proxy (CmpNat x ln))) Refl)
instance (r ~ 'EmptyTree, CmpNat x n ~ 'GT, IsBST l ~ 'True, IsBST r ~ 'True, LtN l n ~ 'True, GtN r n ~ 'True) =>
  ProofIsBSTInsert' x a ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  proofIsBSTInsert' _ (ForkITree _ _ EmptyITree) _ = Refl
instance (r ~ 'ForkTree rl (Node rn rna) rr, CmpNat x n ~ 'GT, IsBST r ~ 'True, IsBST l ~ 'True, GtN r n ~ 'True, LtN l n ~ 'True, ProofIsBSTInsert' x a r (CmpNat x rn),
  ProofGtNInsert' x a r n (CmpNat x rn)) =>
  ProofIsBSTInsert' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT where
  proofIsBSTInsert' node (ForkITree _ _ r@ForkITree{}) _ = gcastWith (proofGtNInsert' node r (Proxy::Proxy n) (Proxy::Proxy (CmpNat x rn))) (gcastWith (proofIsBSTInsert' node r (Proxy::Proxy (CmpNat x rn))) Refl)

class ProofLtNInsert' (x :: Nat) (a :: Type) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofLtNInsert' :: (t ~ 'ForkTree l n1 r, CmpNat x n ~ 'LT, LtN t n ~ 'True) =>
    Node x a -> ITree t -> Proxy n -> Proxy o -> LtN (Insert x a t) n :~: 'True
instance (t ~ 'ForkTree l n1 r, CmpNat x n1 ~ 'EQ, CmpNat x n ~ 'LT, CmpNat n1 n ~ 'LT, LtN l n ~ 'True, LtN r n ~ 'True) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) r) n 'EQ where
  proofLtNInsert' _ ForkITree{} _ _ = Refl
instance (t ~ 'ForkTree l n1 r, l ~ 'EmptyTree, CmpNat x n1 ~ 'LT, CmpNat x n ~ 'LT, CmpNat n1 n ~ 'LT, LtN l n ~ 'True, LtN r n ~ 'True) =>
  ProofLtNInsert' x a ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofLtNInsert' _ (ForkITree EmptyITree _ _) _ _ = Refl
instance (t ~ 'ForkTree l (Node n1 a1) r, l ~ 'ForkTree ll (Node ln lna) lr, CmpNat x n1 ~ 'LT, CmpNat x n ~ 'LT, CmpNat n1 n ~ 'LT, LtN l n ~ 'True, LtN r n ~ 'True,
  ProofLtNInsert' x a l n (CmpNat x ln)) =>
  ProofLtNInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n1 a1) r) n 'LT where
  proofLtNInsert' node (ForkITree l@ForkITree{} _ _) n _ = gcastWith (proofLtNInsert' node l n (Proxy::Proxy (CmpNat x ln))) Refl
instance (t ~ 'ForkTree l (Node n1 a1) r, r ~ 'EmptyTree, CmpNat x n1 ~ 'GT, CmpNat x n ~ 'LT, CmpNat n1 n ~ 'LT, LtN l n ~ 'True, LtN r n ~ 'True) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofLtNInsert' _ (ForkITree _ _ EmptyITree) _ _ = Refl
instance (t ~ 'ForkTree l (Node n1 a1) r, r ~ 'ForkTree rl (Node rn rna) rr, CmpNat x n1 ~ 'GT, CmpNat x n ~ 'LT, CmpNat n1 n ~ 'LT, LtN l n ~ 'True, LtN r n ~ 'True,
  ProofLtNInsert' x a r n (CmpNat x rn)) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn rna) rr)) n 'GT where
  proofLtNInsert' node (ForkITree _ _ r@ForkITree{}) n _ = gcastWith (proofLtNInsert' node r n (Proxy::Proxy (CmpNat x rn))) Refl

class ProofGtNInsert' (x :: Nat) (a :: Type) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofGtNInsert' :: (t ~ 'ForkTree l n1 r, CmpNat x n ~ 'GT, GtN t n ~ 'True) =>
    Node x a -> ITree t -> Proxy n -> Proxy o -> GtN (Insert x a t) n :~: 'True
instance (t ~ 'ForkTree l (Node n1 a1) r, CmpNat x n1 ~ 'EQ, CmpNat x n ~ 'GT, CmpNat n1 n ~ 'GT, GtN l n ~ 'True, GtN r n ~ 'True) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) r) n 'EQ where
  proofGtNInsert' _ ForkITree{} _ _ = Refl
instance (t ~ 'ForkTree l (Node n1 a1) r, l ~ 'EmptyTree, CmpNat x n1 ~ 'LT, CmpNat x n ~ 'GT, CmpNat n1 n ~ 'GT, GtN l n ~ 'True, GtN r n ~ 'True) =>
  ProofGtNInsert' x a ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofGtNInsert' _ (ForkITree EmptyITree _ _) _ _ = Refl
instance (t ~ 'ForkTree l (Node n1 a1) r, l ~ 'ForkTree ll (Node ln lna) lr, CmpNat x n1 ~ 'LT, CmpNat x n ~ 'GT, CmpNat n1 n ~ 'GT, GtN l n ~ 'True, GtN r n ~ 'True,
  ProofGtNInsert' x a l n (CmpNat x ln)) =>
  ProofGtNInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n1 a1) r) n 'LT where
  proofGtNInsert' x (ForkITree l@ForkITree{} _ _) n _ = gcastWith (proofGtNInsert' x l n (Proxy::Proxy (CmpNat x ln))) Refl
instance (t ~ 'ForkTree l (Node n1 a1) r, r ~ 'EmptyTree, CmpNat x n1 ~ 'GT, CmpNat x n ~ 'GT, CmpNat n1 n ~ 'GT, GtN l n ~ 'True, GtN r n ~ 'True) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofGtNInsert' _ (ForkITree _ _ EmptyITree) _ _ = Refl
instance (t ~ 'ForkTree l (Node n1 a1) r, r ~ 'ForkTree rl (Node rn rna) rr, CmpNat x n1 ~ 'GT, CmpNat x n ~ 'GT, CmpNat n1 n ~ 'GT, GtN l n ~ 'True, GtN r n ~ 'True,
  ProofGtNInsert' x a r n (CmpNat x rn)) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn rna) rr)) n 'GT where
  proofGtNInsert' x (ForkITree _ _ r@ForkITree{}) n _ = gcastWith (proofGtNInsert' x r n (Proxy::Proxy (CmpNat x rn))) Refl

insertBST :: (Insertable x a t, ProofIsBSTInsert x a t) =>
  Node x a -> BST t -> BST (Insert x a t)
insertBST x (BST t) = gcastWith (proofIsBSTInsert x t) BST $ insert x t

lookupBST :: (t ~ 'ForkTree l (Node n a1) r, Member x t ~ 'True, Lookupable x a t) =>
  Proxy x -> BST t -> a
lookupBST p (BST t) = lookup p t

class ProofIsBSTDelete (x :: Nat) (t :: Tree) where
  proofIsBSTDelete :: (IsBST t ~ 'True) =>
    Proxy x -> ITree t -> IsBST (Delete x t) :~: 'True
instance ProofIsBSTDelete x 'EmptyTree where
  proofIsBSTDelete _ EmptyITree = Refl
instance ProofIsBSTDelete' x ('ForkTree l (Node n a1) r) (CmpNat x n) =>
  ProofIsBSTDelete x ('ForkTree l (Node n a1) r) where
  proofIsBSTDelete px t@ForkITree{} = proofIsBSTDelete' px t (Proxy::Proxy (CmpNat x n))

class ProofIsBSTDelete' (x :: Nat) (t :: Tree) (o :: Ordering) where
  proofIsBSTDelete' :: (t ~ 'ForkTree l (Node n a1) r) =>
    Proxy x -> ITree t -> Proxy o -> IsBST (Delete' x t o) :~: 'True
instance ProofIsBSTDelete' x ('ForkTree 'EmptyTree (Node n a1) 'EmptyTree) 'EQ where
  proofIsBSTDelete' _ (ForkITree EmptyITree (Node _) EmptyITree) _ = Refl
instance (IsBST rl ~ 'True, IsBST rr ~ 'True, LtN rl rn ~ 'True, GtN rr rn ~ 'True) =>
  ProofIsBSTDelete' x ('ForkTree 'EmptyTree (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  proofIsBSTDelete' _ (ForkITree EmptyITree (Node _) ForkITree{}) _ = Refl
instance (IsBST ll ~ 'True, IsBST lr ~ 'True, LtN ll ln ~ 'True, GtN lr ln ~ 'True) =>
  ProofIsBSTDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) 'EmptyTree) 'EQ where
  proofIsBSTDelete' _ (ForkITree ForkITree{} (Node _) EmptyITree) _ = Refl
instance (IsBST rl ~ 'True, IsBST rr ~ 'True, LtN rl rn ~ 'True, GtN rr rn ~ 'True) =>
  ProofIsBSTDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  -- proofIsBSTDelete' _ (ForkITree ForkITree{} (Node _) ForkITree{}) _ = Refl
instance (IsBST r ~ 'True, GtN r n ~ 'True) =>
  ProofIsBSTDelete' x ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  proofIsBSTDelete' _ (ForkITree EmptyITree (Node _) _) _ = Refl
instance (IsBST r ~ 'True, GtN r n ~ 'True, LtN ('ForkTree ll (Node ln la) lr) n ~ 'True, ProofIsBSTDelete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln),
  CmpNat x n ~ 'LT, ProofLtNDelete' x ('ForkTree ll (Node ln la) lr) n (CmpNat x ln)) =>
  ProofIsBSTDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) r) 'LT where
  proofIsBSTDelete' px (ForkITree l@ForkITree{} _ _) _ = gcastWith (proofLtNDelete' px l (Proxy::Proxy n) (Proxy::Proxy (CmpNat x ln))) (gcastWith (proofIsBSTDelete' px l (Proxy::Proxy (CmpNat x ln))) Refl)
instance (IsBST l ~ 'True, LtN l n ~ 'True) =>
  ProofIsBSTDelete' x ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  proofIsBSTDelete' _ (ForkITree _ (Node _) EmptyITree) _ = Refl
instance (IsBST l ~ 'True, LtN l n ~ 'True, ProofIsBSTDelete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn),
  CmpNat x n ~ 'GT, GtN ('ForkTree rl (Node rn ra) rr) n ~ 'True, ProofGtNDelete' x ('ForkTree rl (Node rn ra) rr) n (CmpNat x rn)) =>
  ProofIsBSTDelete' x ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'GT where
  proofIsBSTDelete' px (ForkITree _ (Node _) r@ForkITree{}) _ = gcastWith (proofGtNDelete' px r (Proxy::Proxy n) (Proxy::Proxy (CmpNat x rn))) (gcastWith (proofIsBSTDelete' px r (Proxy::Proxy (CmpNat x rn))) Refl)

class ProofLtNDelete' (x :: Nat) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofLtNDelete' :: (t ~ 'ForkTree l n1 r, CmpNat x n ~ 'LT, LtN t n ~ 'True) =>
    Proxy x -> ITree t -> Proxy n -> Proxy o -> LtN (Delete' x t o) n :~: 'True
instance ProofLtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) 'EmptyTree) n 'EQ where
  proofLtNDelete' _ (ForkITree EmptyITree (Node _) EmptyITree) _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, IsBST r ~ 'True, LtN r n ~ 'True) =>
  ProofLtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ where
  proofLtNDelete' _ (ForkITree EmptyITree (Node _) ForkITree{}) _ _ = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, IsBST l ~ 'True, LtN l n ~ 'True) =>
  ProofLtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) 'EmptyTree) n 'EQ where
  proofLtNDelete' _ (ForkITree ForkITree{} (Node _) EmptyITree) _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, IsBST r ~ 'True, LtN r n ~ 'True) =>
  ProofLtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ where
  -- proofLtNDelete' _ (ForkITree ForkITree{} (Node _) ForkITree{}) _ _ = Refl
instance (IsBST r ~ 'True, LtN r n ~ 'True) =>
  ProofLtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofLtNDelete' _ (ForkITree EmptyITree (Node _) _) _ _ = Refl
instance (IsBST r ~ 'True, LtN r n ~ 'True, LtN ('ForkTree ll (Node ln la) lr) n ~ 'True, CmpNat n1 n ~ 'LT,
  ProofLtNDelete' x ('ForkTree ll (Node ln la) lr) n (CmpNat x ln)) =>
  ProofLtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) r) n 'LT where
  proofLtNDelete' px (ForkITree l@ForkITree{} _ _) _ _ = gcastWith (proofLtNDelete' px l (Proxy::Proxy n) (Proxy::Proxy (CmpNat x ln))) Refl
instance (IsBST l ~ 'True, LtN l n ~ 'True) =>
  ProofLtNDelete' x ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofLtNDelete' _ (ForkITree _ (Node _) EmptyITree) _ _ = Refl
instance (IsBST l ~ 'True, LtN l n ~ 'True, CmpNat x n1 ~ 'GT, CmpNat n1 n ~ 'LT, LtN ('ForkTree rl (Node rn ra) rr) n ~ 'True,
  ProofLtNDelete' x ('ForkTree rl (Node rn ra) rr) n (CmpNat x rn)) =>
  ProofLtNDelete' x ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'GT where
  proofLtNDelete' px (ForkITree _ (Node _) r@ForkITree{}) _ _ = gcastWith (proofLtNDelete' px r (Proxy::Proxy n) (Proxy::Proxy (CmpNat x rn))) Refl

class ProofGtNDelete' (x :: Nat) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofGtNDelete' :: (t ~ 'ForkTree l n1 r, CmpNat x n ~ 'GT, GtN t n ~ 'True) =>
    Proxy x -> ITree t -> Proxy n -> Proxy o -> GtN (Delete' x t o) n :~: 'True
instance ProofGtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) 'EmptyTree) n 'EQ where
  proofGtNDelete' _ (ForkITree EmptyITree (Node _) EmptyITree) _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, IsBST r ~ 'True, GtN r n ~ 'True) =>
  ProofGtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ where
  proofGtNDelete' _ (ForkITree EmptyITree (Node _) ForkITree{}) _ _ = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, IsBST l ~ 'True, GtN l n ~ 'True) =>
  ProofGtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) 'EmptyTree) n 'EQ where
  proofGtNDelete' _ (ForkITree ForkITree{} (Node _) EmptyITree) _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, IsBST r ~ 'True, GtN r n ~ 'True) =>
  ProofGtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ where
  -- proofGtNDelete' _ (ForkITree ForkITree{} (Node _) ForkITree{}) _ _ = Refl
instance (IsBST r ~ 'True, GtN r n ~ 'True) =>
  ProofGtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofGtNDelete' _ (ForkITree EmptyITree (Node _) _) _ _ = Refl
instance (IsBST r ~ 'True, GtN r n ~ 'True, GtN ('ForkTree ll (Node ln la) lr) n ~ 'True, CmpNat n1 n ~ 'GT,
  ProofGtNDelete' x ('ForkTree ll (Node ln la) lr) n (CmpNat x ln)) =>
  ProofGtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) r) n 'LT where
  proofGtNDelete' px (ForkITree l@ForkITree{} _ _) _ _ = gcastWith (proofGtNDelete' px l (Proxy::Proxy n) (Proxy::Proxy (CmpNat x ln))) Refl
instance (IsBST l ~ 'True, GtN l n ~ 'True) =>
  ProofGtNDelete' x ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofGtNDelete' _ (ForkITree _ (Node _) EmptyITree) _ _ = Refl
instance (IsBST l ~ 'True, GtN l n ~ 'True, CmpNat x n1 ~ 'GT, CmpNat n1 n ~ 'GT, GtN ('ForkTree rl (Node rn ra) rr) n ~ 'True,
  ProofGtNDelete' x ('ForkTree rl (Node rn ra) rr) n (CmpNat x rn)) =>
  ProofGtNDelete' x ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'GT where
  proofGtNDelete' px (ForkITree _ (Node _) r@ForkITree{}) _ _ = gcastWith (proofGtNDelete' px r (Proxy::Proxy n) (Proxy::Proxy (CmpNat x rn))) Refl

deleteBST :: (Deletable x t, ProofIsBSTDelete x t) =>
  Proxy x -> BST t -> BST (Delete x t)
deleteBST px (BST t) = gcastWith (proofIsBSTDelete px t) (BST $ delete px t)
