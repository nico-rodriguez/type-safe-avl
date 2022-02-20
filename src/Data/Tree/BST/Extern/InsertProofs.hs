{-# OPTIONS_HADDOCK ignore-exports #-}

{-|
Module      : Data.Tree.BST.Extern.InsertProofs
Description : Proofs for insertion over externalist BST trees
Copyright   : (c) Nicolás Rodríguez, 2021
License     : GPL-3
Maintainer  : Nicolás Rodríguez
Stability   : experimental
Portability : POSIX

Implementation of the necessary proofs to ensure (at compile time) that the
insertion algorithm defined in "Data.Tree.BST.Extern.Insert" respects the key ordering.
-}

{-# LANGUAGE DataKinds             #-}
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
import           Data.Tree.BST.Extern.Constructors(IsBSTT(EmptyIsBSTT,ForkIsBSTT))
import           Data.Tree.BST.Extern.Insert      (Insertable (Insert),
                                                   Insertable' (Insert'))
import           Data.Tree.BST.Invariants         (GtN, LtN)
import           Data.Tree.ITree                  (Tree (EmptyTree, ForkTree))
import           Data.Tree.Node                   (Node)
import           Data.Type.Equality               ((:~:) (Refl), gcastWith)
import           GHC.TypeNats                     (CmpNat, Nat)
import           Prelude                          (Bool (True), Ordering (EQ, GT, LT), ($))


-- | Prove that inserting a node with key @x@ and element value @a@
-- in a `Data.Tree.BST.Extern.Constructors.BST` tree preserves the @BST@ condition.
class ProofIsBSTInsert (x :: Nat) (a :: Type) (t :: Tree) where
  proofIsBSTInsert :: Proxy (Node x a) -> IsBSTT t -> IsBSTT (Insert x a t)
instance ProofIsBSTInsert x a 'EmptyTree where
  proofIsBSTInsert node _ = ForkIsBSTT EmptyIsBSTT node EmptyIsBSTT
instance (o ~ CmpNat x n,
  ProofIsBSTInsert' x a ('ForkTree l (Node n a1) r) o) =>
  ProofIsBSTInsert x a ('ForkTree l (Node n a1) r) where
  proofIsBSTInsert pNode tIsBST = proofIsBSTInsert' pNode tIsBST (Proxy::Proxy o)

-- | Prove that inserting a node with key @x@ and element value @a@
-- in a `Data.Tree.BST.Extern.Constructors.BST` tree preserves the BST condition, given that the comparison between
-- @x@ and the root key of the tree equals @o@.
-- The @BST@ condition was already checked when `proofIsBSTInsert` was called before.
-- The @o@ parameter guides the proof.
class ProofIsBSTInsert' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) where
  proofIsBSTInsert' :: Proxy (Node x a) -> IsBSTT t -> Proxy o -> IsBSTT (Insert' x a t o)
instance ProofIsBSTInsert' x a ('ForkTree l (Node n a1) r) 'EQ where
  proofIsBSTInsert' _ (ForkIsBSTT l _ r) _ = ForkIsBSTT l pNode r
    where
      pNode = Proxy::Proxy (Node n a)
instance (CmpNat x n ~ 'LT) =>
  ProofIsBSTInsert' x a ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  proofIsBSTInsert' _ (ForkIsBSTT _ pNode' r) _ =
    ForkIsBSTT (ForkIsBSTT EmptyIsBSTT pNode EmptyIsBSTT) pNode' r
      where
        pNode = Proxy::Proxy (Node x a)
instance (l ~ 'ForkTree ll (Node ln lna) lr, o ~ CmpNat x ln,
  CmpNat x n ~ 'LT,
  ProofIsBSTInsert' x a l o, ProofLtNInsert' x a l n o) =>
  ProofIsBSTInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT where
  proofIsBSTInsert' _ (ForkIsBSTT l pNode' r) _ =
    gcastWith (proofLtNInsert' pNode l (Proxy::Proxy n) po) $
    ForkIsBSTT (proofIsBSTInsert' pNode l po) pNode' r
      where
        po    = Proxy::Proxy o
        pNode = Proxy::Proxy (Node x a)
instance (CmpNat x n ~ 'GT) =>
  ProofIsBSTInsert' x a ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  proofIsBSTInsert' _ (ForkIsBSTT l pNode' _) _ =
    ForkIsBSTT l pNode' (ForkIsBSTT EmptyIsBSTT pNode EmptyIsBSTT)
      where
        pNode = Proxy::Proxy (Node x a)
instance (r ~ 'ForkTree rl (Node rn rna) rr, o ~ CmpNat x rn,
  CmpNat x n ~ 'GT,
  ProofIsBSTInsert' x a r o, ProofGtNInsert' x a r n o) =>
  ProofIsBSTInsert' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT where
  proofIsBSTInsert' _ (ForkIsBSTT l pNode' r) _ =
    gcastWith (proofGtNInsert' pNode r (Proxy::Proxy n) po) $
    ForkIsBSTT l pNode' (proofIsBSTInsert' pNode r po)
      where
        po    = Proxy::Proxy o
        pNode = Proxy::Proxy (Node x a)


-- | Prove that inserting a node with key @x@ (lower than @n@) and element value @a@
-- in a tree @t@ which verifies @LtN t n ~ 'True@ preserves the `LtN` invariant,
-- given that the comparison between @x@ and the root key of the tree equals @o@.
-- The @o@ parameter guides the proof.
class ProofLtNInsert' (x :: Nat) (a :: Type) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofLtNInsert' :: (CmpNat x n ~ 'LT, LtN t n ~ 'True) =>
    Proxy (Node x a) -> IsBSTT t -> Proxy n -> Proxy o -> LtN (Insert' x a t o) n :~: 'True
instance ProofLtNInsert' x a ('ForkTree l (Node n1 a1) r) n 'EQ where
  proofLtNInsert' _ _ _ _ = Refl
instance (CmpNat x n1 ~ 'LT) =>
  ProofLtNInsert' x a ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofLtNInsert' _ _ _ _ = Refl
instance (l ~ 'ForkTree ll (Node ln lna) lr, o ~ CmpNat x ln,
  CmpNat x n1 ~ 'LT, LtN l n ~ 'True,
  ProofLtNInsert' x a l n o) =>
  ProofLtNInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n1 a1) r) n 'LT where
  proofLtNInsert' _ (ForkIsBSTT l _ _) pn _ =
    gcastWith (proofLtNInsert' pNode l pn (Proxy::Proxy o)) Refl
      where
          pNode = Proxy::Proxy (Node x a)
instance (CmpNat x n1 ~ 'GT) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofLtNInsert' _ _ _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn rna) rr, o ~ CmpNat x rn,
  CmpNat x n1 ~ 'GT, LtN r n ~ 'True,
  ProofLtNInsert' x a r n o) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn rna) rr)) n 'GT where
  proofLtNInsert' _ (ForkIsBSTT _ _ r) pn _ =
    gcastWith (proofLtNInsert' pNode r pn (Proxy::Proxy o)) Refl
      where
        pNode = Proxy::Proxy (Node x a)


-- | Prove that inserting a node with key @x@ (greater than @n@) and element value @a@
-- in a tree @t@ which verifies @GtN t n ~ 'True@ preserves the `GtN` invariant,
-- given that the comparison between @x@ and the root key of the tree equals @o@.
-- The @o@ parameter guides the proof.
class ProofGtNInsert' (x :: Nat) (a :: Type) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofGtNInsert' :: (CmpNat x n ~ 'GT, GtN t n ~ 'True) =>
     Proxy (Node x a) -> IsBSTT t -> Proxy n -> Proxy o -> GtN (Insert' x a t o) n :~: 'True
instance ProofGtNInsert' x a ('ForkTree l (Node n1 a1) r) n 'EQ where
  proofGtNInsert' _ _ _ _ = Refl
instance (CmpNat x n1 ~ 'LT) =>
  ProofGtNInsert' x a ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofGtNInsert' _ _ _ _ = Refl
instance (l ~ 'ForkTree ll (Node ln lna) lr, o ~ CmpNat x ln,
  CmpNat x n1 ~ 'LT, GtN l n ~ 'True,
  ProofGtNInsert' x a l n o) =>
  ProofGtNInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n1 a1) r) n 'LT where
  proofGtNInsert' node (ForkIsBSTT l _ _) pn _ =
    gcastWith (proofGtNInsert' node l pn (Proxy::Proxy o)) Refl
instance (CmpNat x n1 ~ 'GT) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofGtNInsert' _ _ _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn rna) rr, o ~ CmpNat x rn,
  CmpNat x n1 ~ 'GT, GtN r n ~ 'True,
  ProofGtNInsert' x a r n o) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn rna) rr)) n 'GT where
  proofGtNInsert' node (ForkIsBSTT _ _ r) pn _ =
    gcastWith (proofGtNInsert' node r pn (Proxy::Proxy o)) Refl
