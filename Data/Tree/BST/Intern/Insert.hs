{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Tree.BST.Intern.Insert (
  Insertable(Insert, insert)
) where

import           Data.Kind                        (Type)
import           Data.Proxy                       (Proxy (Proxy))
import           Data.Tree.BST.Intern.Constructor (BST (EmptyBST, ForkBST))
import           Data.Tree.BST.Invariants         (GtN, LtN)
import           Data.Tree.ITree                  (Tree (EmptyTree, ForkTree))
import           Data.Tree.Node                   (Node (Node))
import           Data.Type.Equality               ((:~:) (Refl), gcastWith)
import           GHC.TypeNats                     (CmpNat, Nat)
import           Prelude                          (Bool (True),
                                                   Ordering (EQ, GT, LT), Show,
                                                   ($))


-- | This class provides the functionality to insert a node with key 'x' and value type 'a'
-- | in a BST 't'.
-- | The insertion is defined at the value level and the type level.
-- | The returned tree verifies the BST invariant.
class Insertable (x :: Nat) (a :: Type) (t :: Tree) where
  type Insert (x :: Nat) (a :: Type) (t :: Tree) :: Tree
  insert :: Node x a -> BST t -> BST (Insert x a t)
instance Show a => Insertable x a 'EmptyTree where
  type Insert x a 'EmptyTree = 'ForkTree 'EmptyTree (Node x a) 'EmptyTree
  insert (Node a) EmptyBST         = ForkBST EmptyBST (Node a::Node x a) EmptyBST
instance Insertable' x a ('ForkTree l (Node n a1) r) (CmpNat x n) =>
  Insertable x a ('ForkTree l (Node n a1) r) where
  type Insert x a ('ForkTree l (Node n a1) r) = Insert' x a ('ForkTree l (Node n a1) r) (CmpNat x n)
  insert n t = insert' n t (Proxy::Proxy (CmpNat x n))

-- | This class provides the functionality to insert a node with key 'x' and value type 'a'
-- | in a non empty BST 't'.
-- | It's only used by the 'Insertable' class and it has one extra parameter 'o',
-- | which is the type level comparison of 'x' with the key value of the root node.
-- | The 'o' parameter guides the insertion.
class Insertable' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) where
  type Insert' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) :: Tree
  insert' :: Node x a -> BST t -> Proxy o -> BST (Insert' x a t o)
instance (Show a) => Insertable' x a ('ForkTree l (Node n a1) r) 'EQ where
  type Insert' x a ('ForkTree l (Node n a1) r) 'EQ = 'ForkTree l (Node n a) r
  insert' (Node a) (ForkBST l (Node _) r) _ = ForkBST l (Node a::Node n a) r
instance (Show a, CmpNat x n ~ 'LT) => Insertable' x a ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  type Insert' x a ('ForkTree 'EmptyTree (Node n a1) r) 'LT =
    'ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n a1) r
  insert' (Node a) (ForkBST EmptyBST n r) _ =
    ForkBST (ForkBST EmptyBST (Node a::Node x a) EmptyBST) n r
instance (CmpNat x n ~ 'LT, l ~ 'ForkTree ll (Node ln lna) lr, Insertable' x a l (CmpNat x ln),
  ProofLtNInsert' x a ('ForkTree ll (Node ln lna) lr) n (CmpNat x ln)) =>
  Insertable' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT where
  type Insert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT =
    'ForkTree (Insert' x a ('ForkTree ll (Node ln lna) lr) (CmpNat x ln)) (Node n a1) r
  insert' nx (ForkBST l@ForkBST{} n r) _ =
    gcastWith (proofLtNInsert' nx l (Proxy::Proxy n) (Proxy::Proxy (CmpNat x ln))) $
      ForkBST (insert' nx l (Proxy::Proxy (CmpNat x ln))) n r
instance (Show a, CmpNat x n ~ 'GT) => Insertable' x a ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  type Insert' x a ('ForkTree l (Node n a1) 'EmptyTree) 'GT =
    'ForkTree l (Node n a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree)
  insert' nx (ForkBST l n EmptyBST) _ = ForkBST l n (ForkBST EmptyBST nx EmptyBST)
instance (CmpNat x n ~ 'GT, r ~ 'ForkTree rl (Node rn rna) rr, Insertable' x a r (CmpNat x rn),
  ProofGtNInsert' x a ('ForkTree rl (Node rn rna) rr) n (CmpNat x rn)) =>
  Insertable' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT where
  type Insert' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT =
    'ForkTree l (Node n a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn))
  insert' nx (ForkBST l n r@ForkBST{}) _ =
    gcastWith (proofGtNInsert' nx r (Proxy::Proxy n) (Proxy::Proxy (CmpNat x rn))) $
      ForkBST l n (insert' nx r (Proxy::Proxy (CmpNat x rn)))

-- | Prove that inserting a node with key 'x' (lower than 'n') and element value 'a'
-- | in a BST 't' which verifies 'LtN t n ~ 'True' preserves the LtN invariant,
-- | given that the comparison between 'x' and the root key of the tree equals 'o'.
-- | The 'o' parameter guides the proof.
class ProofLtNInsert' (x :: Nat) (a :: Type) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofLtNInsert' :: (CmpNat x n ~ 'LT, LtN t n ~ 'True) =>
    Node x a -> BST t -> Proxy n -> Proxy o -> LtN (Insert x a t) n :~: 'True
instance (CmpNat x n1 ~ 'EQ) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) r) n 'EQ where
  proofLtNInsert' _ ForkBST{} _ _ = Refl
instance (CmpNat x n1 ~ 'LT) =>
  ProofLtNInsert' x a ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofLtNInsert' _ (ForkBST EmptyBST _ _) _ _ = Refl
instance (CmpNat x n1 ~ 'LT, l ~ 'ForkTree ll (Node ln lna) lr, LtN l n ~ 'True,
  ProofLtNInsert' x a l n (CmpNat x ln)) =>
  ProofLtNInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n1 a1) r) n 'LT where
  proofLtNInsert' node (ForkBST l@ForkBST{} _ _) n _ =
    gcastWith (proofLtNInsert' node l n (Proxy::Proxy (CmpNat x ln))) Refl
instance (CmpNat x n1 ~ 'GT) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofLtNInsert' _ (ForkBST _ _ EmptyBST) _ _ = Refl
instance (CmpNat x n1 ~ 'GT, r ~ 'ForkTree rl (Node rn rna) rr, LtN r n ~ 'True,
  ProofLtNInsert' x a r n (CmpNat x rn)) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn rna) rr)) n 'GT where
  proofLtNInsert' node (ForkBST _ _ r@ForkBST{}) n _ =
    gcastWith (proofLtNInsert' node r n (Proxy::Proxy (CmpNat x rn))) Refl


-- | Prove that inserting a node with key 'x' (greater than 'n') and element value 'a'
-- | in a BST 't' which verifies 'GtN t n ~ 'True' preserves the GtN invariant,
-- | given that the comparison between 'x' and the root key of the tree equals 'o'.
-- | The 'o' parameter guides the proof.
class ProofGtNInsert' (x :: Nat) (a :: Type) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofGtNInsert' :: (CmpNat x n ~ 'GT, GtN t n ~ 'True) =>
    Node x a -> BST t -> Proxy n -> Proxy o -> GtN (Insert x a t) n :~: 'True
instance (CmpNat x n1 ~ 'EQ) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) r) n 'EQ where
  proofGtNInsert' _ ForkBST{} _ _ = Refl
instance (CmpNat x n1 ~ 'LT) =>
  ProofGtNInsert' x a ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofGtNInsert' _ (ForkBST EmptyBST _ _) _ _ = Refl
instance (CmpNat x n1 ~ 'LT, l ~ 'ForkTree ll (Node ln lna) lr, GtN l n ~ 'True,
  ProofGtNInsert' x a l n (CmpNat x ln)) =>
  ProofGtNInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n1 a1) r) n 'LT where
  proofGtNInsert' x (ForkBST l@ForkBST{} _ _) n _ =
    gcastWith (proofGtNInsert' x l n (Proxy::Proxy (CmpNat x ln))) Refl
instance (CmpNat x n1 ~ 'GT) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofGtNInsert' _ (ForkBST _ _ EmptyBST) _ _ = Refl
instance (CmpNat x n1 ~ 'GT, r ~ 'ForkTree rl (Node rn rna) rr, GtN r n ~ 'True,
  ProofGtNInsert' x a r n (CmpNat x rn)) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn rna) rr)) n 'GT where
  proofGtNInsert' x (ForkBST _ _ r@ForkBST{}) n _ =
    gcastWith (proofGtNInsert' x r n (Proxy::Proxy (CmpNat x rn))) Refl
