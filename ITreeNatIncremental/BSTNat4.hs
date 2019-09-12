{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module ITreeNatIncremental.BSTNat4 where

import Data.Kind
import Data.Proxy
import Data.Type.Bool
import Data.Type.Equality
import Data.Nat (Nat(..), Compare, Natty, NATTY(..))
import Prelude hiding (lookup)

data Node :: k -> Type -> Type where
  Node :: forall (k :: Nat) (a :: Type). a -> Node k a
deriving instance Show a => Show (Node k a)

mkNode :: forall (k::Nat)(a::Type). Proxy k -> a -> Node k a
mkNode _ = Node

getValue :: forall (k::Nat)(a::Type). Node k a -> a
getValue (Node a) = a

nod :: Node 'Z Char
nod = mkNode (Proxy::Proxy 'Z) 'c'

val = getValue nod

data Tree :: * where
  EmptyTree :: Tree
  ForkTree  :: Tree -> n -> Tree -> Tree

-- | Check if all elements of the tree are strictly less than x
type family LtN (l :: Tree) (x :: Nat) :: Bool where
  LtN 'EmptyTree        x = 'True
  LtN ('ForkTree l (Node n a) r) x = Compare n x == 'LT && LtN l x && LtN r x

-- | Check if all elements of the tree are strictly greater than x
type family GtN (r :: Tree) (x :: Nat) :: Bool where
  GtN 'EmptyTree        x = 'True
  GtN ('ForkTree l (Node n a) r) x = Compare n x == 'GT && GtN l x && GtN r x

type family IsBST (t :: Tree) :: Bool where
  IsBST 'EmptyTree = 'True
  IsBST ('ForkTree l (Node n a) r) = IsBST l && IsBST r && LtN l n && GtN r n

data ITree :: Tree -> * where
  EmptyITree :: ITree 'EmptyTree
  ForkITree  :: Show a => ITree l -> Node n a -> ITree r -> ITree ('ForkTree l (Node n a) r)

instance Show (ITree t) where
  show EmptyITree         = "E"
  show (ForkITree l n@(Node _) r)  = "F " ++ go l ++ " " ++ show n ++ " " ++ go r
    where
      go :: ITree t' -> String
      go EmptyITree         = "E"
      go (ForkITree l' n'@(Node _) r')  = "(F " ++ go l' ++ " " ++ show n' ++ " " ++ go r' ++ ")"

data BST :: Tree -> Type where
  BST :: (IsBST t ~ 'True) => ITree t -> BST t

instance Show (BST t) where
  show (BST t) = "BST $ " ++ show t

class Insertable (x :: Nat) (a :: Type) (t :: Tree) where
  type Insert (x :: Nat) (a :: Type) (t :: Tree) :: Tree
  insert :: Node x a -> ITree t -> ITree (Insert x a t)
instance Show a => Insertable x a 'EmptyTree where
  type Insert x a 'EmptyTree = 'ForkTree 'EmptyTree (Node x a) 'EmptyTree
  insert (Node a) EmptyITree         = ForkITree EmptyITree (Node a::Node x a) EmptyITree
instance Insertable' x a ('ForkTree l (Node n a1) r) (Compare x n) => Insertable x a ('ForkTree l (Node n a1) r) where
  type Insert x a ('ForkTree l (Node n a1) r) = Insert' x a ('ForkTree l (Node n a1) r) (Compare x n)
  insert n t = insert' n t (Proxy::Proxy (Compare x n))

class Insertable' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) where
  type Insert' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) :: Tree
  insert' :: Node x a -> ITree t -> Proxy o -> ITree (Insert x a t)
instance (Show a, Compare x n ~ 'EQ) => Insertable' x a ('ForkTree l (Node n a1) r) 'EQ where
  type Insert' x a ('ForkTree l (Node n a1) r) 'EQ = 'ForkTree l (Node n a) r
  insert' (Node a) (ForkITree l (Node _) r) _ = ForkITree l (Node a::Node n a) r
instance (Show a, Compare x n ~ 'LT) => Insertable' x a ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  type Insert' x a ('ForkTree 'EmptyTree (Node n a1) r) 'LT = 'ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n a1) r
  insert' (Node a) (ForkITree EmptyITree n r) _ = ForkITree (ForkITree EmptyITree (Node a::Node x a) EmptyITree) n r
instance (Compare x n ~ 'LT, l ~ 'ForkTree ll (Node ln lna) lr, Insertable' x a l (Compare x ln)) => Insertable' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT where
  type Insert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT = 'ForkTree (Insert' x a ('ForkTree ll (Node ln lna) lr) (Compare x ln)) (Node n a1) r
  insert' (Node a) (ForkITree l@ForkITree{} n r) _ = ForkITree (insert' (Node a::Node x a) l (Proxy::Proxy (Compare x ln))) n r
instance (Show a, Compare x n ~ 'GT) => Insertable' x a ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  type Insert' x a ('ForkTree l (Node n a1) 'EmptyTree) 'GT = 'ForkTree l (Node n a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree)
  insert' (Node a) (ForkITree l n EmptyITree) _ = ForkITree l n (ForkITree EmptyITree (Node a::Node x a) EmptyITree)
instance (Compare x n ~ 'GT, r ~ 'ForkTree rl (Node rn rna) rr, Insertable' x a r (Compare x rn)) => Insertable' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT where
  type Insert' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT = 'ForkTree l (Node n a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) (Compare x rn))
  insert' (Node a) (ForkITree l n r@ForkITree{}) _ = ForkITree l n (insert' (Node a::Node x a) r (Proxy::Proxy (Compare x rn)))

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

type family Member (x :: Nat) (t :: Tree) :: Bool where
  Member x 'EmptyTree = 'False
  Member x ('ForkTree l (Node n a) r) =
    (If (Compare x n == 'EQ)
      'True
      (If (Compare x n == 'LT)
        (Member x l)
        (Member x r)
      )
    )

type family LookupValueType (x :: Nat) (t :: Tree) :: Type where
  LookupValueType x ('ForkTree l (Node n a) r) =
    (If (Compare x n == 'EQ)
      a
      (If (Compare x n == 'LT)
        (LookupValueType x l)
        (LookupValueType x r)
      )
    )

class Lookupable (x :: Nat) (a :: Type) (t :: Tree) where
  lookup :: (t ~ 'ForkTree l (Node n a1) r, Member x t ~ 'True) =>
    Proxy x -> ITree t -> a
instance (Lookupable' x a ('ForkTree l (Node n a1) r) (Compare x n), a ~ LookupValueType x ('ForkTree l (Node n a1) r)) =>
  Lookupable x a ('ForkTree l (Node n a1) r) where
  lookup x t = lookup' x t (Proxy::Proxy (Compare x n))

class Lookupable' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) where
  lookup' :: (t ~ 'ForkTree l (Node n a1) r, Member x t ~ 'True) =>
    Proxy x -> ITree t -> Proxy o -> a
instance (Compare x n ~ 'EQ) => Lookupable' x a ('ForkTree l (Node n a) r) 'EQ where
  lookup' _ (ForkITree _ (Node a) _) _ = getValue (Node a::Node n a)
instance (Compare x n ~ 'LT, l ~ 'ForkTree ll (Node ln lna) lr, Member x l ~ 'True, Lookupable' x a l (Compare x ln)) =>
  Lookupable' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT where
  lookup' p (ForkITree l@ForkITree{} _ _) _ = lookup' p l (Proxy::Proxy (Compare x ln))
instance (Compare x n ~ 'GT, r ~ 'ForkTree rl (Node rn rna) rr, Member x r ~ 'True, Lookupable' x a ('ForkTree rl (Node rn rna) rr) (Compare x rn)) =>
  Lookupable' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT where
  lookup' p (ForkITree _ _ r@ForkITree{}) _ = lookup' p r (Proxy::Proxy (Compare x rn))

lookupBST :: (t ~ 'ForkTree l (Node n a1) r, Member x t ~ 'True, Lookupable x a t) =>
  Proxy x -> BST t -> a
lookupBST p (BST t) = lookup p t
