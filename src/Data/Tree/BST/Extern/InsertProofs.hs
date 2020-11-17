{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# LANGUAGE Safe                  #-}

module Data.Tree.BST.Extern.InsertProofs (
  ProofIsBSTInsert(proofIsBSTInsert)
) where

import           Data.Kind                        (Type)
import           Data.Proxy                       (Proxy (Proxy))
import           Data.Tree.BST.Extern.Constructor (BST (BST))
import           Data.Tree.BST.Extern.Insert      (Insertable (Insert),
                                                   Insertable' (Insert'))
import           Data.Tree.BST.Invariants         (GtN, LtN, IsBSTT(EmptyIsBSTT,ForkIsBSTT))
import           Data.Tree.ITree                  (Tree (EmptyTree, ForkTree))
import           Data.Tree.Node                   (Node)
import           Data.Type.Equality               ((:~:) (Refl), gcastWith)
import           GHC.TypeNats                     (CmpNat, Nat)
import           Prelude                          (Bool (True),
                                                   Ordering (EQ, GT, LT), ($))


-- | Prove that inserting a node with key 'x' and element value 'a'
-- | in a BST tree preserves the BST condition.
class ProofIsBSTInsert (x :: Nat) (a :: Type) (t :: Tree) where
  proofIsBSTInsert :: Node x a -> BST t -> IsBSTT (Insert x a t)
instance ProofIsBSTInsert x a 'EmptyTree where
  proofIsBSTInsert node (BST _ EmptyIsBSTT) = ForkIsBSTT EmptyIsBSTT node EmptyIsBSTT
instance ProofIsBSTInsert' x a ('ForkTree l (Node n a1) r) (CmpNat x n) =>
  ProofIsBSTInsert x a ('ForkTree l (Node n a1) r) where
  proofIsBSTInsert node (BST _ tIsBST) = proofIsBSTInsert' node tIsBST (Proxy::Proxy (CmpNat x n))

-- | Prove that inserting a node with key 'x' and element value 'a'
-- | in a BST tree preserves the BST condition, given that the comparison between
-- | 'x' and the root key of the tree equals 'o'.
-- | The BST invariant was already check when proofIsBSTInsert was called before.
-- | The 'o' parameter guides the proof.
class ProofIsBSTInsert' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) where
  proofIsBSTInsert' :: Node x a -> IsBSTT t -> Proxy o -> IsBSTT (Insert' x a t o)
instance (x ~ n) => ProofIsBSTInsert' x a ('ForkTree l (Node n a1) r) 'EQ where
  proofIsBSTInsert' node (ForkIsBSTT l _ r) _ = ForkIsBSTT l node r
instance (CmpNat x n ~ 'LT) =>
  ProofIsBSTInsert' x a ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  proofIsBSTInsert' node (ForkIsBSTT EmptyIsBSTT node' r) _ =
    ForkIsBSTT (ForkIsBSTT EmptyIsBSTT node EmptyIsBSTT) node' r
instance (l ~ 'ForkTree ll (Node ln lna) lr, CmpNat x n ~ 'LT,
  ProofIsBSTInsert' x a l (CmpNat x ln), ProofLtNInsert' x a l n (CmpNat x ln)) =>
  ProofIsBSTInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT where
  proofIsBSTInsert' node (ForkIsBSTT l@ForkIsBSTT{} node' r) _ =
    gcastWith (proofLtNInsert' node l (Proxy::Proxy n) (Proxy::Proxy (CmpNat x ln))) $
    ForkIsBSTT (proofIsBSTInsert' node l (Proxy::Proxy (CmpNat x ln))) node' r
instance (CmpNat x n ~ 'GT) =>
  ProofIsBSTInsert' x a ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  proofIsBSTInsert' node (ForkIsBSTT l node' EmptyIsBSTT) _ =
    ForkIsBSTT l node' (ForkIsBSTT EmptyIsBSTT node EmptyIsBSTT)
instance (r ~ 'ForkTree rl (Node rn rna) rr, CmpNat x n ~ 'GT,
  ProofIsBSTInsert' x a r (CmpNat x rn), ProofGtNInsert' x a r n (CmpNat x rn)) =>
  ProofIsBSTInsert' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT where
  proofIsBSTInsert' node (ForkIsBSTT l node' r@ForkIsBSTT{}) _ =
    gcastWith (proofGtNInsert' node r (Proxy::Proxy n) (Proxy::Proxy (CmpNat x rn))) $
    ForkIsBSTT l node' (proofIsBSTInsert' node r (Proxy::Proxy (CmpNat x rn)))


-- | Prove that inserting a node with key 'x' (lower than 'n') and element value 'a'
-- | in a tree 't' which verifies 'LtN t n ~ 'True' preserves the LtN invariant,
-- | given that the comparison between 'x' and the root key of the tree equals 'o'.
-- | The 'o' parameter guides the proof.
class ProofLtNInsert' (x :: Nat) (a :: Type) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofLtNInsert' :: (CmpNat x n ~ 'LT, LtN t n ~ 'True) =>
    Node x a -> IsBSTT t -> Proxy n -> Proxy o -> LtN (Insert' x a t o) n :~: 'True
instance (CmpNat x n1 ~ 'EQ) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) r) n 'EQ where
  proofLtNInsert' _ ForkIsBSTT{} _ _ = Refl
instance (CmpNat x n1 ~ 'LT) =>
  ProofLtNInsert' x a ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofLtNInsert' _ (ForkIsBSTT EmptyIsBSTT _ _) _ _ = Refl
instance (CmpNat x n1 ~ 'LT, l ~ 'ForkTree ll (Node ln lna) lr,
  ProofLtNInsert' x a l n (CmpNat x ln), LtN l n ~ 'True) =>
  ProofLtNInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n1 a1) r) n 'LT where
  proofLtNInsert' node (ForkIsBSTT l@ForkIsBSTT{} _ _) pn _ =
    gcastWith (proofLtNInsert' node l pn (Proxy::Proxy (CmpNat x ln))) Refl
instance (CmpNat x n1 ~ 'GT) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofLtNInsert' _ (ForkIsBSTT _ _ EmptyIsBSTT) _ _ = Refl
instance (CmpNat x n1 ~ 'GT, r ~ 'ForkTree rl (Node rn rna) rr,
  ProofLtNInsert' x a r n (CmpNat x rn), LtN r n ~ 'True) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn rna) rr)) n 'GT where
  proofLtNInsert' node (ForkIsBSTT _ _ r@ForkIsBSTT{}) pn _ =
    gcastWith (proofLtNInsert' node r pn (Proxy::Proxy (CmpNat x rn))) Refl


-- | Prove that inserting a node with key 'x' (greater than 'n') and element value 'a'
-- | in a tree 't' which verifies 'GtN t n ~ 'True' preserves the GtN invariant,
-- | given that the comparison between 'x' and the root key of the tree equals 'o'.
-- | The 'o' parameter guides the proof.
class ProofGtNInsert' (x :: Nat) (a :: Type) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofGtNInsert' :: (CmpNat x n ~ 'GT, GtN t n ~ 'True) =>
    Node x a -> IsBSTT t -> Proxy n -> Proxy o -> GtN (Insert' x a t o) n :~: 'True
instance (CmpNat x n1 ~ 'EQ) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) r) n 'EQ where
  proofGtNInsert' _ ForkIsBSTT{} _ _ = Refl
instance (CmpNat x n1 ~ 'LT) =>
  ProofGtNInsert' x a ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofGtNInsert' _ (ForkIsBSTT EmptyIsBSTT _ _) _ _ = Refl
instance (CmpNat x n1 ~ 'LT, l ~ 'ForkTree ll (Node ln lna) lr,
  ProofGtNInsert' x a l n (CmpNat x ln), GtN l n ~ 'True) =>
  ProofGtNInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n1 a1) r) n 'LT where
  proofGtNInsert' x (ForkIsBSTT l@ForkIsBSTT{} _ _) pn _ =
    gcastWith (proofGtNInsert' x l pn (Proxy::Proxy (CmpNat x ln))) Refl
instance (CmpNat x n1 ~ 'GT) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofGtNInsert' _ (ForkIsBSTT _ _ EmptyIsBSTT) _ _ = Refl
instance (CmpNat x n1 ~ 'GT, r ~ 'ForkTree rl (Node rn rna) rr,
  ProofGtNInsert' x a r n (CmpNat x rn), GtN r n ~ 'True) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn rna) rr)) n 'GT where
  proofGtNInsert' x (ForkIsBSTT _ _ r@ForkIsBSTT{}) pn _ =
    gcastWith (proofGtNInsert' x r pn (Proxy::Proxy (CmpNat x rn))) Refl
