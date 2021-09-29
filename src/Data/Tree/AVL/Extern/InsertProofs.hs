{-# OPTIONS_HADDOCK ignore-exports #-}

{-|
Module      : Data.Tree.BST.Extern.InsertProofs
Description : Proofs for insertion over externalist AVL trees
Copyright   : (c) Nicolás Rodríguez, 2021
License     : GPL-3
Maintainer  : Nicolás Rodríguez
Stability   : experimental
Portability : POSIX

Implementation of the necessary proofs to ensure (at compile time) that the
insertion algorithm defined in "Data.Tree.AVL.Extern.Insert" respects the key ordering and height balance restrictions.
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

module Data.Tree.AVL.Extern.InsertProofs (
  ProofIsAVLInsert(proofIsAVLInsert),
  ProofIsBSTInsert(proofIsBSTInsert)
) where

import           Data.Kind                          (Type)
import           Data.Proxy                         (Proxy (Proxy))
import           Data.Tree.AVL.Extern.BalanceProofs (ProofGtNBalance (proofGtNBalance),
                                                     ProofIsAVLBalance (proofIsAVLBalance),
                                                     ProofIsBSTBalance (proofIsBSTBalance),
                                                     ProofLtNBalance (proofLtNBalance))
import           Data.Tree.AVL.Extern.Insert        (Insert',
                                                     Insertable (Insert))
import           Data.Tree.AVL.Extern.Constructors  (IsAVLT(EmptyIsAVLT,ForkIsAVLT), IsAlmostAVLT(ForkIsAlmostAVLT))
import           Data.Tree.BST.Invariants           (GtN, LtN)
import           Data.Tree.BST.Extern.Constructors  (IsBSTT(EmptyIsBSTT,ForkIsBSTT))
import           Data.Tree.ITree                    (Tree (EmptyTree, ForkTree))
import           Data.Tree.Node                     (Node)
import           Data.Type.Equality                 ((:~:) (Refl), gcastWith)
import           GHC.TypeNats                       (CmpNat, Nat)
import           Prelude                            (Bool (True), undefined,
                                                     Ordering (EQ, GT, LT), ($))


-- | Prove that inserting a node with key 'x' and element value 'a'
-- in a `BST` tree preserves `BST` condition.
class ProofIsBSTInsert (x :: Nat) (a :: Type) (t :: Tree) where
  proofIsBSTInsert :: Node x a -> IsBSTT t -> IsBSTT (Insert x a t)
instance ProofIsBSTInsert x a 'EmptyTree where
  proofIsBSTInsert _ _ = ForkIsBSTT EmptyIsBSTT node EmptyIsBSTT
    where
        node = undefined::Node x a
instance ProofIsBSTInsert' x a ('ForkTree l (Node n a1) r) (CmpNat x n) =>
  ProofIsBSTInsert x a ('ForkTree l (Node n a1) r) where
  proofIsBSTInsert _ tIsBST = proofIsBSTInsert' node tIsBST (Proxy::Proxy (CmpNat x n))
    where
        node = undefined::Node x a

-- | Prove that inserting a node with key 'x' and element value 'a'
-- in a `BST` tree preserves `BST` condition, given that the comparison between
-- 'x' and the root key of the tree equals 'o'.
-- The `BST` restrictions were already checked when `proofIsBSTInsert` was called before.
-- The 'o' parameter guides the proof.
class ProofIsBSTInsert' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) where
  proofIsBSTInsert' :: Node x a -> IsBSTT t -> Proxy o -> IsBSTT (Insert' x a t o)
instance ProofIsBSTInsert' x a ('ForkTree l (Node n a1) r) 'EQ where
  proofIsBSTInsert' _ (ForkIsBSTT lIsBST _ rIsBST) _ = ForkIsBSTT lIsBST node rIsBST
    where
        node = undefined::Node n a
instance (CmpNat x n ~ 'LT,
  ProofIsBSTBalance ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n a1) r)) =>
  ProofIsBSTInsert' x a ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  proofIsBSTInsert' _ (ForkIsBSTT _ node' rIsBST) _ =
    proofIsBSTBalance $ ForkIsBSTT (ForkIsBSTT EmptyIsBSTT node EmptyIsBSTT) node' rIsBST
      where
          node = undefined::Node x a
instance (l ~ 'ForkTree ll (Node ln lna) lr, o ~ CmpNat x ln,
  CmpNat x n ~ 'LT,
  ProofIsBSTInsert' x a l o, ProofLtNInsert' x a l n o,
  ProofIsBSTBalance ('ForkTree (Insert' x a l o) (Node n a1) r)) =>
  ProofIsBSTInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT where
  proofIsBSTInsert' _ (ForkIsBSTT lIsBST node' rIsBST) _ =
    gcastWith (proofLtNInsert' node lIsBST (Proxy::Proxy n) (Proxy::Proxy o)) $
    proofIsBSTBalance $ ForkIsBSTT (proofIsBSTInsert node lIsBST) node' rIsBST
      where
        node = undefined::Node x a
instance (CmpNat x n ~ 'GT,
  ProofIsBSTBalance ('ForkTree l (Node n a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree))) =>
  ProofIsBSTInsert' x a ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  proofIsBSTInsert' _ (ForkIsBSTT lIsBST node' _) _ =
    proofIsBSTBalance (ForkIsBSTT lIsBST node' (ForkIsBSTT EmptyIsBSTT node EmptyIsBSTT))
      where
        node = undefined::Node x a
instance (r ~ 'ForkTree rl (Node rn rna) rr, o ~ CmpNat x rn,
  CmpNat x n ~ 'GT,
  ProofGtNInsert' x a r n o, ProofIsBSTInsert' x a r o,
  ProofIsBSTBalance ('ForkTree l (Node n a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) o))) =>
  ProofIsBSTInsert' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT where
  proofIsBSTInsert' _ (ForkIsBSTT lIsBST node' rIsBST) _ =
    gcastWith (proofGtNInsert' node rIsBST (Proxy::Proxy n) (Proxy::Proxy o)) $
    proofIsBSTBalance $ ForkIsBSTT lIsBST node' (proofIsBSTInsert node rIsBST)
      where
        node = undefined::Node x a


-- | Prove that inserting a node with key 'x' (lower than 'n') and element value 'a'
-- in a tree 't' which verifies @LtN t n ~ 'True@ preserves the `LtN` invariant,
-- given that the comparison between 'x' and the root key of the tree equals 'o'.
-- The 'o' parameter guides the proof.
class ProofLtNInsert' (x :: Nat) (a :: Type) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofLtNInsert' :: (CmpNat x n ~ 'LT, LtN t n ~ 'True) =>
    Node x a -> IsBSTT t -> Proxy n -> Proxy o -> LtN (Insert' x a t o) n :~: 'True
instance ProofLtNInsert' x a ('ForkTree l (Node n1 a1) r) n 'EQ where
  proofLtNInsert' _ _ _ _ = Refl
instance (CmpNat x n1 ~ 'LT,
  ProofLtNBalance ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n1 a1) r) n) =>
  ProofLtNInsert' x a ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofLtNInsert' _ (ForkIsBSTT _ node' rIsBST) pn _ =
    gcastWith (proofLtNBalance (ForkIsBSTT (ForkIsBSTT EmptyIsBSTT node EmptyIsBSTT) node' rIsBST) pn) Refl
      where
        node = undefined::Node x a
instance (l ~ 'ForkTree ll (Node ln lna) lr, o ~ CmpNat x ln,
  CmpNat x n1 ~ 'LT, LtN l n ~ 'True,
  ProofLtNInsert' x a l n o, ProofLtNInsert' x a l n1 o, ProofIsBSTInsert x a l,
  ProofLtNBalance ('ForkTree (Insert' x a l o) (Node n1 a1) r) n) =>
  ProofLtNInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n1 a1) r) n 'LT where
  proofLtNInsert' _ (ForkIsBSTT lIsBST node' rIsBST) pn _ =
    gcastWith (proofLtNInsert' node lIsBST pn po) $
    gcastWith (proofLtNInsert' node lIsBST (Proxy::Proxy n1) po) $
    gcastWith (proofLtNBalance (ForkIsBSTT lIsBST' node' rIsBST) pn) Refl
      where
        po      = Proxy::Proxy o
        lIsBST' = proofIsBSTInsert node lIsBST
        node    = undefined::Node x a
instance (CmpNat x n1 ~ 'GT,
  ProofLtNBalance ('ForkTree l (Node n1 a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree)) n) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofLtNInsert' _ (ForkIsBSTT lIsBST node' _) pn _ =
    gcastWith (proofLtNBalance (ForkIsBSTT lIsBST node' (ForkIsBSTT EmptyIsBSTT node EmptyIsBSTT)) pn) Refl
      where
        node = undefined::Node x a
instance (r ~ 'ForkTree rl (Node rn rna) rr, o ~ CmpNat x rn,
  CmpNat n1 n ~ 'LT, CmpNat x n1 ~ 'GT, LtN r n ~ 'True,
  ProofLtNInsert' x a r n o, ProofGtNInsert' x a r n1 o, ProofIsBSTInsert x a r,
  ProofLtNBalance ('ForkTree l (Node n1 a1) (Insert' x a r o)) n) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn rna) rr)) n 'GT where
  proofLtNInsert' _ (ForkIsBSTT lIsBST node' rIsBST) pn _ =
    gcastWith (proofLtNInsert' node rIsBST pn po) $
    gcastWith (proofGtNInsert' node rIsBST (Proxy::Proxy n1) po) $
    gcastWith (proofLtNBalance (ForkIsBSTT lIsBST node' rIsBST') pn) Refl
      where
        po      = Proxy::Proxy o
        rIsBST' = proofIsBSTInsert node rIsBST
        node    = undefined::Node x a


-- | Prove that inserting a node with key 'x' (greater than 'n') and element value 'a'
-- in a tree 't' which verifies @GtN t n ~ 'True@ preserves the `GtN` invariant,
-- given that the comparison between 'x' and the root key of the tree equals 'o'.
-- The 'o' parameter guides the proof.
class ProofGtNInsert' (x :: Nat) (a :: Type) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofGtNInsert' :: (CmpNat x n ~ 'GT, GtN t n ~ 'True) =>
    Node x a -> IsBSTT t -> Proxy n -> Proxy o -> GtN (Insert' x a t o) n :~: 'True
instance ProofGtNInsert' x a ('ForkTree l (Node n1 a1) r) n 'EQ where
  proofGtNInsert' _ _ _ _ = Refl
instance (CmpNat x n1 ~ 'LT,
  ProofGtNBalance ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n1 a1) r) n) =>
  ProofGtNInsert' x a ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofGtNInsert' _ (ForkIsBSTT _ node' rIsBST) pn _ =
    gcastWith (proofGtNBalance (ForkIsBSTT (ForkIsBSTT EmptyIsBSTT node EmptyIsBSTT) node' rIsBST) pn) Refl
      where
        node = undefined::Node x a
instance (l ~ 'ForkTree ll (Node ln lna) lr, o ~ CmpNat x ln,
  CmpNat x n1 ~ 'LT, GtN l n ~ 'True,
  ProofGtNInsert' x a l n o, ProofLtNInsert' x a l n1 o, ProofIsBSTInsert x a l,
  ProofGtNBalance ('ForkTree (Insert' x a l o) (Node n1 a1) r) n) =>
  ProofGtNInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n1 a1) r) n 'LT where
  proofGtNInsert' _ (ForkIsBSTT lIsBST node' rIsBST) pn _ =
    gcastWith (proofGtNInsert' node lIsBST pn po) $
    gcastWith (proofLtNInsert' node lIsBST (Proxy::Proxy n1) po) $
    gcastWith (proofGtNBalance (ForkIsBSTT lIsBST' node' rIsBST) pn) Refl
      where
        po      = Proxy::Proxy o
        lIsBST' = proofIsBSTInsert node lIsBST
        node    = undefined::Node x a
instance (CmpNat x n1 ~ 'GT,
  ProofGtNBalance ('ForkTree l (Node n1 a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree)) n) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofGtNInsert' _ (ForkIsBSTT lIsBST node' _) pn _ =
    gcastWith (proofGtNBalance (ForkIsBSTT lIsBST node' (ForkIsBSTT EmptyIsBSTT node EmptyIsBSTT)) pn) Refl
      where
        node = undefined::Node x a
instance (r ~ 'ForkTree rl (Node rn rna) rr, o ~ CmpNat x rn,
  CmpNat x n1 ~ 'GT, GtN r n ~ 'True,
  ProofGtNInsert' x a r n o, ProofGtNInsert' x a r n1 o, ProofIsBSTInsert x a r,
  ProofGtNBalance ('ForkTree l (Node n1 a1) (Insert' x a r o)) n) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn rna) rr)) n 'GT where
  proofGtNInsert' _ (ForkIsBSTT lIsBST node' rIsBST) pn _ =
    gcastWith (proofGtNInsert' node rIsBST pn po) $
    gcastWith (proofGtNInsert' node rIsBST (Proxy::Proxy n1) po) $
    gcastWith (proofGtNBalance (ForkIsBSTT lIsBST node' rIsBST') pn) Refl
      where
        po      = Proxy::Proxy o
        rIsBST' = proofIsBSTInsert node rIsBST
        node    = undefined::Node x a


-- | Prove that inserting a node with key 'x' and element value 'a'
-- in an `AVL` tree preserves the `AVL` condition.
class ProofIsAVLInsert (x :: Nat) (a :: Type) (t :: Tree) where
  proofIsAVLInsert :: Node x a -> IsAVLT t -> IsAVLT (Insert x a t)
instance ProofIsAVLInsert x a 'EmptyTree where
  proofIsAVLInsert _ _ = ForkIsAVLT EmptyIsAVLT node EmptyIsAVLT
    where
        node = undefined::Node x a
instance (o ~ CmpNat x n, ProofIsAVLInsert' x a ('ForkTree l (Node n a1) r) o) =>
  ProofIsAVLInsert x a ('ForkTree l (Node n a1) r) where
  proofIsAVLInsert _ tIsAVL = proofIsAVLInsert' node tIsAVL (Proxy::Proxy o)
    where
        node = undefined::Node x a

-- | Prove that inserting a node with key 'x' and element value 'a'
-- in an `AVL` tree preserves the `AVL` condition, given that the comparison between
-- 'x' and the root key of the tree equals 'o'.
-- The `AVL` condition was already checked when `proofIsBSTInsert` was called before.
-- The 'o' parameter guides the proof.
class ProofIsAVLInsert' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) where
  proofIsAVLInsert' :: Node x a -> IsAVLT t -> Proxy o -> IsAVLT (Insert' x a t o)
instance ProofIsAVLInsert' x a ('ForkTree l (Node n a1) r) 'EQ where
  proofIsAVLInsert' _ (ForkIsAVLT lIsAVL _ rIsAVL) _ = ForkIsAVLT lIsAVL node rIsAVL
    where
      node  = undefined::Node n a
instance (ProofIsAVLBalance ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n a1) r)) =>
  ProofIsAVLInsert' x a ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  proofIsAVLInsert' _ (ForkIsAVLT _ node' rIsAVL) _ =
    proofIsAVLBalance (ForkIsAlmostAVLT (ForkIsAVLT EmptyIsAVLT node EmptyIsAVLT) node' rIsAVL)
      where
        node = undefined::Node x a
instance (l ~ 'ForkTree ll (Node ln lna) lr, o ~ CmpNat x ln,
  ProofIsAVLInsert' x a l o, ProofIsAVLBalance ('ForkTree (Insert' x a l o) (Node n a1) r)) =>
  ProofIsAVLInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT where
  proofIsAVLInsert' _ (ForkIsAVLT lIsAVL node' rIsAVL) _ =
    proofIsAVLBalance $ ForkIsAlmostAVLT lIsAVL' node' rIsAVL
      where
        lIsAVL' = proofIsAVLInsert node lIsAVL
        node    = undefined::Node x a
instance (ProofIsAVLBalance ('ForkTree l (Node n a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree))) =>
  ProofIsAVLInsert' x a ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  proofIsAVLInsert' _ (ForkIsAVLT lIsAVL node' _) _ =
    proofIsAVLBalance $ ForkIsAlmostAVLT lIsAVL node' (ForkIsAVLT EmptyIsAVLT node EmptyIsAVLT)
      where
        node = undefined::Node x a
instance (r ~ 'ForkTree rl (Node rn rna) rr, o ~ CmpNat x rn,
  ProofIsAVLInsert' x a r o, ProofIsAVLBalance ('ForkTree l (Node n a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) o))) =>
  ProofIsAVLInsert' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT where
  proofIsAVLInsert' _ (ForkIsAVLT lIsAVL node' rIsAVL) _ =
    proofIsAVLBalance $ ForkIsAlmostAVLT lIsAVL node' rIsAVL'
      where
        rIsAVL' = proofIsAVLInsert node rIsAVL
        node    = undefined::Node x a
