{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Tree.AVL.Extern.InsertProofs (
  AVL(AVL),
  IsAVL,
  ProofIsAVLInsert(proofIsAVLInsert),
  ProofIsBSTInsert(proofIsBSTInsert)
) where

import           Data.Kind                          (Type)
import           Data.Proxy                         (Proxy (Proxy))
import           Data.Tree.AVL.Extern.BalanceProofs (ProofGtNBalance (proofGtNBalance),
                                                     ProofIsAVLBalance (proofIsAVLBalance),
                                                     ProofIsBSTBalance (proofIsBSTBalance),
                                                     ProofLtNBalance (proofLtNBalance))
import           Data.Tree.AVL.Extern.Constructor   (AVL (AVL))
import           Data.Tree.AVL.Extern.Insert        (Insert',
                                                     Insertable (Insert))
import           Data.Tree.AVL.Invariants           (IsAVL)
import           Data.Tree.AVL.InvariantsProofs     (proofIsAVLLeftSubTree,
                                                     proofIsAVLRightSubTree)
import           Data.Tree.BST.Extern.Constructor   (BST (BST))
import           Data.Tree.BST.Invariants           (GtN, IsBST, LtN)
import           Data.Tree.BST.InvariantsProofs     (proofGtNLeftSubTree,
                                                     proofGtNRightSubTree,
                                                     proofIsBSTGtN,
                                                     proofIsBSTLeftSubTree,
                                                     proofIsBSTLtN,
                                                     proofIsBSTRightSubTree,
                                                     proofLtNLeftSubTree,
                                                     proofLtNRightSubTree)
import           Data.Tree.ITree                    (ITree (EmptyITree, ForkITree),
                                                     Tree (EmptyTree, ForkTree))
import           Data.Tree.Node                     (Node)
import           Data.Type.Equality                 ((:~:) (Refl), gcastWith)
import           GHC.TypeNats                       (CmpNat, Nat)
import           Prelude                            (Bool (True),
                                                     Ordering (EQ, GT, LT), ($))


-- | Prove that inserting a node with key 'x' and element value 'a'
-- | in a BST tree preserves BST condition.
class ProofIsBSTInsert (x :: Nat) (a :: Type) (t :: Tree) where
  proofIsBSTInsert :: Node x a -> BST t -> IsBST (Insert x a t) :~: 'True
instance ProofIsBSTInsert x a 'EmptyTree where
  proofIsBSTInsert _ (BST EmptyITree) = Refl
instance ProofIsBSTInsert' x a ('ForkTree l (Node n a1) r) (CmpNat x n) =>
  ProofIsBSTInsert x a ('ForkTree l (Node n a1) r) where
  proofIsBSTInsert node (BST t) = proofIsBSTInsert' node t (Proxy::Proxy (CmpNat x n))

-- | Prove that inserting a node with key 'x' and element value 'a'
-- | in a BST tree preserves BST condition, given that the comparison between
-- | 'x' and the root key of the tree equals 'o'.
-- | The BST invariant was already check when proofIsBSTInsert was called before.
-- | The 'o' parameter guides the proof.
class ProofIsBSTInsert' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) where
  proofIsBSTInsert' :: (IsBST t ~ 'True) =>
    Node x a -> ITree t -> Proxy o -> IsBST (Insert' x a t o) :~: 'True
instance ProofIsBSTInsert' x a ('ForkTree l (Node n a1) r) 'EQ where
  proofIsBSTInsert' _ ForkITree{} _ = Refl
instance (CmpNat x n ~ 'LT, ProofIsBSTBalance ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n a1) r)) =>
  ProofIsBSTInsert' x a ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  proofIsBSTInsert' _ (ForkITree EmptyITree _ _) _ =
    gcastWith (proofIsBSTBalance (Proxy::Proxy ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n a1) r))) Refl
instance (l ~ 'ForkTree ll (Node ln lna) lr, CmpNat x n ~ 'LT,
  ProofIsBSTInsert' x a l (CmpNat x ln), ProofLtNInsert' x a l n (CmpNat x ln),
  ProofIsBSTBalance ('ForkTree (Insert' x a l (CmpNat x ln)) (Node n a1) r)) =>
  ProofIsBSTInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT where
  proofIsBSTInsert' node (ForkITree l@ForkITree{} _ _) _ =
    gcastWith (proofIsBSTLtN pt Refl) $
    gcastWith (proofLtNInsert' node l (Proxy::Proxy n) (Proxy::Proxy (CmpNat x ln))) $
    gcastWith (proofIsBSTGtN pt Refl) $
    gcastWith (proofIsBSTRightSubTree pt Refl) $
    gcastWith (proofIsBSTInsert' node l (Proxy::Proxy (CmpNat x ln))) $
    gcastWith (proofIsBSTBalance (Proxy::Proxy ('ForkTree (Insert' x a l (CmpNat x ln)) (Node n a1) r))) Refl
    where
      pt = Proxy::Proxy ('ForkTree l (Node n a1) r)
instance (CmpNat x n ~ 'GT, ProofIsBSTBalance ('ForkTree l (Node n a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree))) =>
  ProofIsBSTInsert' x a ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  proofIsBSTInsert' _ (ForkITree _ _ EmptyITree) _ =
    gcastWith (proofIsBSTBalance (Proxy::Proxy ('ForkTree l (Node n a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree)))) Refl
instance (r ~ 'ForkTree rl (Node rn rna) rr, CmpNat x n ~ 'GT,
  ProofGtNInsert' x a r n (CmpNat x rn), ProofIsBSTInsert' x a r (CmpNat x rn),
  ProofIsBSTBalance ('ForkTree l (Node n a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn)))) =>
  ProofIsBSTInsert' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT where
  proofIsBSTInsert' node (ForkITree _ _ r@ForkITree{}) _ =
    gcastWith (proofIsBSTGtN pt Refl) $
    gcastWith (proofGtNInsert' node r (Proxy::Proxy n) (Proxy::Proxy (CmpNat x rn))) $
    gcastWith (proofIsBSTLtN pt Refl) $
    gcastWith (proofIsBSTLeftSubTree pt Refl) $
    gcastWith (proofIsBSTInsert' node r (Proxy::Proxy (CmpNat x rn))) $
    gcastWith (proofIsBSTBalance (Proxy::Proxy ('ForkTree l (Node n a1) (Insert' x a r (CmpNat x rn))))) Refl
    where
      pt = Proxy::Proxy ('ForkTree l (Node n a1) r)


-- | Prove that inserting a node with key 'x' (lower than 'n') and element value 'a'
-- | in a tree 't' which verifies 'LtN t n ~ 'True' preserves the LtN invariant,
-- | given that the comparison between 'x' and the root key of the tree equals 'o'.
-- | The 'o' parameter guides the proof.
class ProofLtNInsert' (x :: Nat) (a :: Type) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofLtNInsert' :: (CmpNat x n ~ 'LT, LtN t n ~ 'True) =>
    Node x a -> ITree t -> Proxy n -> Proxy o -> LtN (Insert' x a t o) n :~: 'True
instance ProofLtNInsert' x a ('ForkTree l (Node n1 a1) r) n 'EQ where
  proofLtNInsert' _ ForkITree{} _ _ = Refl
instance (ProofLtNBalance ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n1 a1) r) n) =>
  ProofLtNInsert' x a ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofLtNInsert' _ (ForkITree EmptyITree _ _) pn _ =
    gcastWith (proofLtNBalance (Proxy::Proxy ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n1 a1) r)) pn) Refl
instance (l ~ 'ForkTree ll (Node ln lna) lr, ProofLtNInsert' x a l n (CmpNat x ln),
  ProofLtNBalance ('ForkTree (Insert' x a l (CmpNat x ln)) (Node n1 a1) r) n) =>
  ProofLtNInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n1 a1) r) n 'LT where
  proofLtNInsert' node (ForkITree l@ForkITree{} _ _) pn _ =
    gcastWith (proofLtNLeftSubTree (Proxy::Proxy ('ForkTree l (Node n1 a1) r)) pn Refl) $
    gcastWith (proofLtNInsert' node l pn (Proxy::Proxy (CmpNat x ln))) $
    gcastWith (proofLtNBalance (Proxy::Proxy ('ForkTree (Insert' x a l (CmpNat x ln)) (Node n1 a1) r)) pn) Refl
instance (ProofLtNBalance ('ForkTree l (Node n1 a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree)) n) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofLtNInsert' _ (ForkITree _ _ EmptyITree) pn _ =
    gcastWith (proofLtNBalance (Proxy::Proxy ('ForkTree l (Node n1 a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree))) pn) Refl
instance (r ~ 'ForkTree rl (Node rn rna) rr, ProofLtNInsert' x a r n (CmpNat x rn),
  ProofLtNBalance ('ForkTree l (Node n1 a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn))) n) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn rna) rr)) n 'GT where
  proofLtNInsert' node (ForkITree _ _ r@ForkITree{}) pn _ =
    gcastWith (proofLtNRightSubTree (Proxy::Proxy ('ForkTree l (Node n1 a1) r)) pn Refl) $
    gcastWith (proofLtNInsert' node r pn (Proxy::Proxy (CmpNat x rn))) $
    gcastWith (proofLtNBalance (Proxy::Proxy ('ForkTree l (Node n1 a1) (Insert' x a r (CmpNat x rn)))) pn) Refl


-- | Prove that inserting a node with key 'x' (greater than 'n') and element value 'a'
-- | in a tree 't' which verifies 'GtN t n ~ 'True' preserves the GtN invariant,
-- | given that the comparison between 'x' and the root key of the tree equals 'o'.
-- | The 'o' parameter guides the proof.
class ProofGtNInsert' (x :: Nat) (a :: Type) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofGtNInsert' :: (CmpNat x n ~ 'GT, GtN t n ~ 'True) =>
    Node x a -> ITree t -> Proxy n -> Proxy o -> GtN (Insert' x a t o) n :~: 'True
instance ProofGtNInsert' x a ('ForkTree l (Node n1 a1) r) n 'EQ where
  proofGtNInsert' _ ForkITree{} _ _ = Refl
instance (ProofGtNBalance ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n1 a1) r) n) =>
  ProofGtNInsert' x a ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofGtNInsert' _ (ForkITree EmptyITree _ _) _ _ =
    gcastWith (proofGtNBalance (Proxy::Proxy ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n1 a1) r)) (Proxy::Proxy n)) Refl
instance (l ~ 'ForkTree ll (Node ln lna) lr, ProofGtNInsert' x a l n (CmpNat x ln),
  ProofGtNBalance ('ForkTree (Insert' x a l (CmpNat x ln)) (Node n1 a1) r) n) =>
  ProofGtNInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n1 a1) r) n 'LT where
  proofGtNInsert' x (ForkITree l@ForkITree{} _ _) pn _ =
    gcastWith (proofGtNLeftSubTree (Proxy::Proxy ('ForkTree l (Node n1 a1) r)) pn Refl) $
    gcastWith (proofGtNInsert' x l pn (Proxy::Proxy (CmpNat x ln))) $
    gcastWith (proofGtNBalance (Proxy::Proxy ('ForkTree (Insert' x a l (CmpNat x ln)) (Node n1 a1) r)) pn) Refl
instance (ProofGtNBalance ('ForkTree l (Node n1 a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree)) n) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofGtNInsert' _ (ForkITree _ _ EmptyITree) pn _ =
    gcastWith (proofGtNBalance (Proxy::Proxy ('ForkTree l (Node n1 a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree))) pn) Refl
instance (r ~ 'ForkTree rl (Node rn rna) rr, ProofGtNInsert' x a r n (CmpNat x rn),
  ProofGtNBalance ('ForkTree l (Node n1 a1) (Insert' x a r (CmpNat x rn))) n) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn rna) rr)) n 'GT where
  proofGtNInsert' x (ForkITree _ _ r@ForkITree{}) pn _ =
    gcastWith (proofGtNRightSubTree (Proxy::Proxy ('ForkTree l (Node n1 a1) r)) pn Refl) $
    gcastWith (proofGtNInsert' x r pn (Proxy::Proxy (CmpNat x rn))) $
    gcastWith (proofGtNBalance (Proxy::Proxy ('ForkTree l (Node n1 a1) (Insert' x a r (CmpNat x rn)))) pn) Refl


-- | Prove that inserting a node with key 'x' and element value 'a'
-- | in an AVL tree preserves the AVL condition.
class ProofIsAVLInsert (x :: Nat) (a :: Type) (t :: Tree) where
  proofIsAVLInsert :: Node x a -> AVL t -> IsAVL (Insert x a t) :~: 'True
instance ProofIsAVLInsert x a 'EmptyTree where
  proofIsAVLInsert _ (AVL EmptyITree) = Refl
instance ProofIsAVLInsert' x a ('ForkTree l (Node n a1) r) (CmpNat x n) => ProofIsAVLInsert x a ('ForkTree l (Node n a1) r) where
  proofIsAVLInsert node (AVL t) = proofIsAVLInsert' node t (Proxy::Proxy (CmpNat x n))

-- | Prove that inserting a node with key 'x' and element value 'a'
-- | in an AVL tree preserves the AVL condition, given that the comparison between
-- | 'x' and the root key of the tree equals 'o'.
-- | The AVL invariant was already check when proofIsBSTInsert was called before.
-- | The 'o' parameter guides the proof.
class ProofIsAVLInsert' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) where
  proofIsAVLInsert' :: (IsAVL t ~ 'True) =>
    Node x a -> ITree t -> Proxy o -> IsAVL (Insert' x a t o) :~: 'True
instance ProofIsAVLInsert' x a ('ForkTree l (Node n a1) r) 'EQ where
  proofIsAVLInsert' _ ForkITree{} _ = Refl
instance (ProofIsAVLBalance ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n a1) r)) =>
  ProofIsAVLInsert' x a ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  proofIsAVLInsert' _ (ForkITree EmptyITree _ _) _ =
    gcastWith (proofIsAVLBalance (Proxy::Proxy ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n a1) r))) Refl
instance (l ~ 'ForkTree ll (Node ln lna) lr, ProofIsAVLInsert' x a l (CmpNat x ln),
  ProofIsAVLBalance ('ForkTree (Insert' x a l (CmpNat x ln)) (Node n a1) r)) =>
  ProofIsAVLInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT where
  proofIsAVLInsert' node (ForkITree l@ForkITree{} _ _) _ =
    gcastWith (proofIsAVLLeftSubTree (Proxy::Proxy ('ForkTree l (Node n a1) r)) Refl) $
    gcastWith (proofIsAVLInsert' node l (Proxy::Proxy (CmpNat x ln))) $
    gcastWith (proofIsAVLBalance (Proxy::Proxy ('ForkTree (Insert' x a l (CmpNat x ln)) (Node n a1) r))) Refl
instance (ProofIsAVLBalance ('ForkTree l (Node n a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree))) =>
  ProofIsAVLInsert' x a ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  proofIsAVLInsert' _ (ForkITree _ _ EmptyITree) _ =
    gcastWith (proofIsAVLBalance (Proxy::Proxy ('ForkTree l (Node n a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree)))) Refl
instance (r ~ 'ForkTree rl (Node rn rna) rr, ProofIsAVLInsert' x a r (CmpNat x rn),
  ProofIsAVLBalance ('ForkTree l (Node n a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn)))) =>
  ProofIsAVLInsert' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT where
  proofIsAVLInsert' node (ForkITree _ _ r@ForkITree{}) _ =
    gcastWith (proofIsAVLRightSubTree (Proxy::Proxy ('ForkTree l (Node n a1) r)) Refl) $
    gcastWith (proofIsAVLInsert' node r (Proxy::Proxy (CmpNat x rn))) $
    gcastWith (proofIsAVLBalance (Proxy::Proxy ('ForkTree l (Node n a1) (Insert' x a r (CmpNat x rn))))) Refl
