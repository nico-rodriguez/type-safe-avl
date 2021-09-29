{-# OPTIONS_HADDOCK ignore-exports #-}

{-|
Module      : Data.Tree.BST.Intern.Insert
Description : Insertion algorithm (with proofs) over internalist BST trees
Copyright   : (c) Nicolás Rodríguez, 2021
License     : GPL-3
Maintainer  : Nicolás Rodríguez
Stability   : experimental
Portability : POSIX

Implementation of the insertion algorithm over internalist BST trees,
along with the necessary proofs to ensure (at compile time) that the
key ordering still holds.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# LANGUAGE Safe                  #-}

module Data.Tree.BST.Intern.Insert (
  Insertable(Insert, insert)
) where

import           Data.Kind                        (Type)
import           Data.Proxy                       (Proxy (Proxy))
import           Data.Tree.BST.Intern.Constructors(BST (EmptyBST, ForkBST))
import           Data.Tree.BST.Invariants         (GtN, LtN)
import           Data.Tree.ITree                  (Tree (EmptyTree, ForkTree))
import           Data.Tree.Node                   (Node, mkNode, getValue)
import           Data.Type.Equality               ((:~:) (Refl), gcastWith)
import           GHC.TypeNats                     (CmpNat, Nat)
import           Prelude                          (Bool (True), Ordering (EQ, GT, LT),
                                                   Show, ($))


-- | This class provides the functionality to insert a node with key 'x' and value type 'a'
-- in a `BST` 't'.
-- The insertion is defined at the value level and the type level.
-- The returned tree verifies the `BST` restrictions.
class Insertable (x :: Nat) (a :: Type) (t :: Tree) where
  type Insert (x :: Nat) (a :: Type) (t :: Tree) :: Tree
  insert :: Node x a -> BST t -> BST (Insert x a t)
instance (Show a) =>
  Insertable x a 'EmptyTree where
  type Insert x a 'EmptyTree = 'ForkTree 'EmptyTree (Node x a) 'EmptyTree
  insert node _ = ForkBST EmptyBST node EmptyBST
instance (o ~ CmpNat x n,
  Insertable' x a ('ForkTree l (Node n a1) r) o) =>
  Insertable x a ('ForkTree l (Node n a1) r) where
  type Insert x a ('ForkTree l (Node n a1) r) = Insert' x a ('ForkTree l (Node n a1) r) (CmpNat x n)
  insert n t = insert' n t (Proxy::Proxy o)

-- | This class provides the functionality to insert a node with key 'x' and value type 'a'
-- in a non empty `BST` 't'.
-- It's only used by the 'Insertable' class and it has one extra parameter 'o',
-- which is the type level comparison of 'x' with the key value of the root node.
-- The 'o' parameter guides the insertion.
class Insertable' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) where
  type Insert' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) :: Tree
  insert' :: Node x a -> BST t -> Proxy o -> BST (Insert' x a t o)
instance (Show a) =>
  Insertable' x a ('ForkTree l (Node n a1) r) 'EQ where
  type Insert' x a ('ForkTree l (Node n a1) r) 'EQ = 'ForkTree l (Node n a) r
  insert' node (ForkBST l _ r) _ = ForkBST l node' r
    where
      node' = mkNode (Proxy::Proxy n) (getValue node)
instance (CmpNat x n ~ 'LT,
  Show a) =>
  Insertable' x a ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  type Insert' x a ('ForkTree 'EmptyTree (Node n a1) r) 'LT =
    'ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n a1) r
  insert' node (ForkBST _ node' r) _ =
    ForkBST (ForkBST EmptyBST node EmptyBST) node' r
instance (l ~ 'ForkTree ll (Node ln lna) lr, o ~ CmpNat x ln,
  CmpNat x n ~ 'LT,
  Insertable' x a l o,
  ProofLtNInsert' x a ('ForkTree ll (Node ln lna) lr) n o) =>
  Insertable' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT where
  type Insert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT =
    'ForkTree (Insert' x a ('ForkTree ll (Node ln lna) lr) (CmpNat x ln)) (Node n a1) r
  insert' node (ForkBST l node' r) _ =
    gcastWith (proofLtNInsert' node l (Proxy::Proxy n) po) $
    ForkBST (insert' node l po) node' r
      where
        po = Proxy::Proxy (CmpNat x ln)
instance (CmpNat x n ~ 'GT,
  Show a) =>
  Insertable' x a ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  type Insert' x a ('ForkTree l (Node n a1) 'EmptyTree) 'GT =
    'ForkTree l (Node n a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree)
  insert' node (ForkBST l node' _) _ = ForkBST l node' (ForkBST EmptyBST node EmptyBST)
instance (r ~ 'ForkTree rl (Node rn rna) rr, o ~ CmpNat x rn,
  CmpNat x n ~ 'GT,
  Insertable' x a r o,
  ProofGtNInsert' x a ('ForkTree rl (Node rn rna) rr) n o) =>
  Insertable' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT where
  type Insert' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT =
    'ForkTree l (Node n a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn))
  insert' node (ForkBST l node' r) _ =
    gcastWith (proofGtNInsert' node r (Proxy::Proxy n) po) $
    ForkBST l node' (insert' node r po)
      where
        po = Proxy::Proxy o

-- | Prove that inserting a node with key 'x' (lower than 'n') and element value 'a'
-- in a `BST` 't' which verifies @LtN t n ~ 'True@ preserves the `LtN` invariant,
-- given that the comparison between 'x' and the root key of the tree equals 'o'.
-- The 'o' parameter guides the proof.
class ProofLtNInsert' (x :: Nat) (a :: Type) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofLtNInsert' :: (CmpNat x n ~ 'LT, LtN t n ~ 'True) =>
    Node x a -> BST t -> Proxy n -> Proxy o -> LtN (Insert x a t) n :~: 'True
instance (CmpNat x n1 ~ 'EQ) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) r) n 'EQ where
  proofLtNInsert' _ _ _ _ = Refl
instance (CmpNat x n1 ~ 'LT) =>
  ProofLtNInsert' x a ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofLtNInsert' _ _ _ _ = Refl
instance (l ~ 'ForkTree ll (Node ln lna) lr, o ~ CmpNat x ln,
  CmpNat x n1 ~ 'LT, LtN l n ~ 'True,
  ProofLtNInsert' x a l n o) =>
  ProofLtNInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n1 a1) r) n 'LT where
  proofLtNInsert' node (ForkBST l _ _) pn _ =
    gcastWith (proofLtNInsert' node l pn (Proxy::Proxy o)) Refl
instance (CmpNat x n1 ~ 'GT) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofLtNInsert' _ _ _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn rna) rr, o ~ CmpNat x rn,
  CmpNat x n1 ~ 'GT, LtN r n ~ 'True,
  ProofLtNInsert' x a r n o) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn rna) rr)) n 'GT where
  proofLtNInsert' node (ForkBST _ _ r) pn _ =
    gcastWith (proofLtNInsert' node r pn (Proxy::Proxy o)) Refl


-- | Prove that inserting a node with key 'x' (greater than 'n') and element value 'a'
-- in a `BST` 't' which verifies @GtN t n ~ 'True@ preserves the `GtN` invariant,
-- given that the comparison between 'x' and the root key of the tree equals 'o'.
-- The 'o' parameter guides the proof.
class ProofGtNInsert' (x :: Nat) (a :: Type) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofGtNInsert' :: (CmpNat x n ~ 'GT, GtN t n ~ 'True) =>
    Node x a -> BST t -> Proxy n -> Proxy o -> GtN (Insert x a t) n :~: 'True
instance (CmpNat x n1 ~ 'EQ) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) r) n 'EQ where
  proofGtNInsert' _ _ _ _ = Refl
instance (CmpNat x n1 ~ 'LT) =>
  ProofGtNInsert' x a ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofGtNInsert' _ _ _ _ = Refl
instance (l ~ 'ForkTree ll (Node ln lna) lr, o ~ CmpNat x ln,
  CmpNat x n1 ~ 'LT, GtN l n ~ 'True,
  ProofGtNInsert' x a l n o) =>
  ProofGtNInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n1 a1) r) n 'LT where
  proofGtNInsert' node (ForkBST l _ _) pn _ =
    gcastWith (proofGtNInsert' node l pn (Proxy::Proxy o)) Refl
instance (CmpNat x n1 ~ 'GT) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofGtNInsert' _ _ _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn rna) rr, o ~ CmpNat x rn,
  CmpNat x n1 ~ 'GT, GtN r n ~ 'True,
  ProofGtNInsert' x a r n o) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn rna) rr)) n 'GT where
  proofGtNInsert' node (ForkBST _ _ r) pn _ =
    gcastWith (proofGtNInsert' node r pn (Proxy::Proxy o)) Refl
