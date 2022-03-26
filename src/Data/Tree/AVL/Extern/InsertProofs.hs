{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

-- |
-- Module      : Data.Tree.BST.Extern.InsertProofs
-- Description : Proofs for insertion over externalist AVL trees
-- Copyright   : (c) Nicolás Rodríguez, 2021
-- License     : GPL-3
-- Maintainer  : Nicolás Rodríguez
-- Stability   : experimental
-- Portability : POSIX
--
-- Implementation of the necessary proofs to ensure (at compile time) that the
-- insertion algorithm defined in "Data.Tree.AVL.Extern.Insert" respects the key ordering and height balance restrictions.
module Data.Tree.AVL.Extern.InsertProofs
  ( ProofIsBalancedInsert (proofIsBalancedInsert),
    ProofIsBSTInsert (proofIsBSTInsert),
  )
where

import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Tree.AVL.Extern.BalanceProofs
  ( ProofGtNBalance (proofGtNBalance),
    ProofIsBSTBalance (proofIsBSTBalance),
    ProofIsBalancedBalance (proofIsBalancedBalance),
    ProofLtNBalance (proofLtNBalance),
  )
import Data.Tree.AVL.Extern.Constructors (IsAlmostBalancedT (ForkIsAlmostBalancedT), IsBalancedT (EmptyIsBalancedT, ForkIsBalancedT))
import Data.Tree.AVL.Extern.Insert
  ( Insert',
    Insertable (Insert),
  )
import Data.Tree.BST.Extern.Constructors (IsBSTT (EmptyIsBSTT, ForkIsBSTT))
import Data.Tree.BST.Invariants (GtN, LtN)
import Data.Tree.ITree (Tree (EmptyTree, ForkTree))
import Data.Tree.Node (Node)
import Data.Type.Equality (gcastWith, (:~:) (Refl))
import GHC.TypeNats (CmpNat, Nat)
import Prelude (Bool (True), Ordering (EQ, GT, LT), ($))

-- | Prove that inserting a node with key @x@ and element value @a@
-- in a `Data.Tree.BST.Extern.Constructors.BST` tree preserves @BST@ condition.
class ProofIsBSTInsert (x :: Nat) (a :: Type) (t :: Tree) where
  proofIsBSTInsert :: Proxy (Node x a) -> IsBSTT t -> IsBSTT (Insert x a t)

instance ProofIsBSTInsert x a 'EmptyTree where
  proofIsBSTInsert pNode _ = ForkIsBSTT EmptyIsBSTT pNode EmptyIsBSTT

instance
  ProofIsBSTInsert' x a ('ForkTree l (Node n a1) r) (CmpNat x n) =>
  ProofIsBSTInsert x a ('ForkTree l (Node n a1) r)
  where
  proofIsBSTInsert pNode tIsBST = proofIsBSTInsert' pNode tIsBST (Proxy :: Proxy (CmpNat x n))

-- | Prove that inserting a node with key @x@ and element value @a@
-- in a `Data.Tree.BST.Extern.Constructors.BST` tree preserves @BST@ condition, given that the comparison between
-- @x@ and the root key of the tree equals @o@.
-- The @BST@ restrictions were already checked when `proofIsBSTInsert` was called before.
-- The @o@ parameter guides the proof.
class ProofIsBSTInsert' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) where
  proofIsBSTInsert' :: Proxy (Node x a) -> IsBSTT t -> Proxy o -> IsBSTT (Insert' x a t o)

instance ProofIsBSTInsert' x a ('ForkTree l (Node n a1) r) 'EQ where
  proofIsBSTInsert' _ (ForkIsBSTT lIsBST _ rIsBST) _ = ForkIsBSTT lIsBST pNode rIsBST
    where
      pNode = Proxy :: Proxy (Node n a)

instance
  ( CmpNat x n ~ 'LT,
    ProofIsBSTBalance ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n a1) r)
  ) =>
  ProofIsBSTInsert' x a ('ForkTree 'EmptyTree (Node n a1) r) 'LT
  where
  proofIsBSTInsert' pNode (ForkIsBSTT _ pNode' rIsBST) _ =
    proofIsBSTBalance $ ForkIsBSTT (ForkIsBSTT EmptyIsBSTT pNode EmptyIsBSTT) pNode' rIsBST

instance
  ( l ~ 'ForkTree ll (Node ln lna) lr,
    o ~ CmpNat x ln,
    CmpNat x n ~ 'LT,
    ProofIsBSTInsert' x a l o,
    ProofLtNInsert' x a l n o,
    ProofIsBSTBalance ('ForkTree (Insert' x a l o) (Node n a1) r)
  ) =>
  ProofIsBSTInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT
  where
  proofIsBSTInsert' pNode (ForkIsBSTT lIsBST pNode' rIsBST) _ =
    gcastWith (proofLtNInsert' pNode lIsBST (Proxy :: Proxy n) (Proxy :: Proxy o)) $
      proofIsBSTBalance $ ForkIsBSTT (proofIsBSTInsert pNode lIsBST) pNode' rIsBST

instance
  ( CmpNat x n ~ 'GT,
    ProofIsBSTBalance ('ForkTree l (Node n a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree))
  ) =>
  ProofIsBSTInsert' x a ('ForkTree l (Node n a1) 'EmptyTree) 'GT
  where
  proofIsBSTInsert' pNode (ForkIsBSTT lIsBST pNode' _) _ =
    proofIsBSTBalance (ForkIsBSTT lIsBST pNode' (ForkIsBSTT EmptyIsBSTT pNode EmptyIsBSTT))

instance
  ( r ~ 'ForkTree rl (Node rn rna) rr,
    o ~ CmpNat x rn,
    CmpNat x n ~ 'GT,
    ProofGtNInsert' x a r n o,
    ProofIsBSTInsert' x a r o,
    ProofIsBSTBalance ('ForkTree l (Node n a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) o))
  ) =>
  ProofIsBSTInsert' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT
  where
  proofIsBSTInsert' pNode (ForkIsBSTT lIsBST pNode' rIsBST) _ =
    gcastWith (proofGtNInsert' pNode rIsBST (Proxy :: Proxy n) (Proxy :: Proxy o)) $
      proofIsBSTBalance $ ForkIsBSTT lIsBST pNode' (proofIsBSTInsert pNode rIsBST)

-- | Prove that inserting a node with key @x@ (lower than @n@) and element value @a@
-- in a tree @t@ which verifies @LtN t n ~ 'True@ preserves the `LtN` invariant,
-- given that the comparison between @x@ and the root key of the tree equals @o@.
-- The @o@ parameter guides the proof.
class ProofLtNInsert' (x :: Nat) (a :: Type) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofLtNInsert' ::
    (CmpNat x n ~ 'LT, LtN t n ~ 'True) =>
    Proxy (Node x a) ->
    IsBSTT t ->
    Proxy n ->
    Proxy o ->
    LtN (Insert' x a t o) n :~: 'True

instance ProofLtNInsert' x a ('ForkTree l (Node n1 a1) r) n 'EQ where
  proofLtNInsert' _ _ _ _ = Refl

instance
  ( CmpNat x n1 ~ 'LT,
    ProofLtNBalance ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n1 a1) r) n
  ) =>
  ProofLtNInsert' x a ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT
  where
  proofLtNInsert' pNode (ForkIsBSTT _ pNode' rIsBST) pn _ =
    gcastWith (proofLtNBalance (ForkIsBSTT (ForkIsBSTT EmptyIsBSTT pNode EmptyIsBSTT) pNode' rIsBST) pn) Refl

instance
  ( l ~ 'ForkTree ll (Node ln lna) lr,
    o ~ CmpNat x ln,
    CmpNat x n1 ~ 'LT,
    LtN l n ~ 'True,
    ProofLtNInsert' x a l n o,
    ProofLtNInsert' x a l n1 o,
    ProofIsBSTInsert x a l,
    ProofLtNBalance ('ForkTree (Insert' x a l o) (Node n1 a1) r) n
  ) =>
  ProofLtNInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n1 a1) r) n 'LT
  where
  proofLtNInsert' pNode (ForkIsBSTT lIsBST pNode' rIsBST) pn _ =
    gcastWith (proofLtNInsert' pNode lIsBST pn po) $
      gcastWith (proofLtNInsert' pNode lIsBST (Proxy :: Proxy n1) po) $
        gcastWith (proofLtNBalance (ForkIsBSTT lIsBST' pNode' rIsBST) pn) Refl
    where
      po = Proxy :: Proxy o
      lIsBST' = proofIsBSTInsert pNode lIsBST

instance
  ( CmpNat x n1 ~ 'GT,
    ProofLtNBalance ('ForkTree l (Node n1 a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree)) n
  ) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT
  where
  proofLtNInsert' pNode (ForkIsBSTT lIsBST pNode' _) pn _ =
    gcastWith (proofLtNBalance (ForkIsBSTT lIsBST pNode' (ForkIsBSTT EmptyIsBSTT pNode EmptyIsBSTT)) pn) Refl

instance
  ( r ~ 'ForkTree rl (Node rn rna) rr,
    o ~ CmpNat x rn,
    CmpNat n1 n ~ 'LT,
    CmpNat x n1 ~ 'GT,
    LtN r n ~ 'True,
    ProofLtNInsert' x a r n o,
    ProofGtNInsert' x a r n1 o,
    ProofIsBSTInsert x a r,
    ProofLtNBalance ('ForkTree l (Node n1 a1) (Insert' x a r o)) n
  ) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn rna) rr)) n 'GT
  where
  proofLtNInsert' pNode (ForkIsBSTT lIsBST pNode' rIsBST) pn _ =
    gcastWith (proofLtNInsert' pNode rIsBST pn po) $
      gcastWith (proofGtNInsert' pNode rIsBST (Proxy :: Proxy n1) po) $
        gcastWith (proofLtNBalance (ForkIsBSTT lIsBST pNode' rIsBST') pn) Refl
    where
      po = Proxy :: Proxy o
      rIsBST' = proofIsBSTInsert pNode rIsBST

-- | Prove that inserting a node with key @x@ (greater than @n@) and element value @a@
-- in a tree @t@ which verifies @GtN t n ~ 'True@ preserves the `GtN` invariant,
-- given that the comparison between @x@ and the root key of the tree equals @o@.
-- The @o@ parameter guides the proof.
class ProofGtNInsert' (x :: Nat) (a :: Type) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofGtNInsert' ::
    (CmpNat x n ~ 'GT, GtN t n ~ 'True) =>
    Proxy (Node x a) ->
    IsBSTT t ->
    Proxy n ->
    Proxy o ->
    GtN (Insert' x a t o) n :~: 'True

instance ProofGtNInsert' x a ('ForkTree l (Node n1 a1) r) n 'EQ where
  proofGtNInsert' _ _ _ _ = Refl

instance
  ( CmpNat x n1 ~ 'LT,
    ProofGtNBalance ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n1 a1) r) n
  ) =>
  ProofGtNInsert' x a ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT
  where
  proofGtNInsert' pNode (ForkIsBSTT _ pNode' rIsBST) pn _ =
    gcastWith (proofGtNBalance (ForkIsBSTT (ForkIsBSTT EmptyIsBSTT pNode EmptyIsBSTT) pNode' rIsBST) pn) Refl

instance
  ( l ~ 'ForkTree ll (Node ln lna) lr,
    o ~ CmpNat x ln,
    CmpNat x n1 ~ 'LT,
    GtN l n ~ 'True,
    ProofGtNInsert' x a l n o,
    ProofLtNInsert' x a l n1 o,
    ProofIsBSTInsert x a l,
    ProofGtNBalance ('ForkTree (Insert' x a l o) (Node n1 a1) r) n
  ) =>
  ProofGtNInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n1 a1) r) n 'LT
  where
  proofGtNInsert' pNode (ForkIsBSTT lIsBST pNode' rIsBST) pn _ =
    gcastWith (proofGtNInsert' pNode lIsBST pn po) $
      gcastWith (proofLtNInsert' pNode lIsBST (Proxy :: Proxy n1) po) $
        gcastWith (proofGtNBalance (ForkIsBSTT lIsBST' pNode' rIsBST) pn) Refl
    where
      po = Proxy :: Proxy o
      lIsBST' = proofIsBSTInsert pNode lIsBST

instance
  ( CmpNat x n1 ~ 'GT,
    ProofGtNBalance ('ForkTree l (Node n1 a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree)) n
  ) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT
  where
  proofGtNInsert' pNode (ForkIsBSTT lIsBST pNode' _) pn _ =
    gcastWith (proofGtNBalance (ForkIsBSTT lIsBST pNode' (ForkIsBSTT EmptyIsBSTT pNode EmptyIsBSTT)) pn) Refl

instance
  ( r ~ 'ForkTree rl (Node rn rna) rr,
    o ~ CmpNat x rn,
    CmpNat x n1 ~ 'GT,
    GtN r n ~ 'True,
    ProofGtNInsert' x a r n o,
    ProofGtNInsert' x a r n1 o,
    ProofIsBSTInsert x a r,
    ProofGtNBalance ('ForkTree l (Node n1 a1) (Insert' x a r o)) n
  ) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn rna) rr)) n 'GT
  where
  proofGtNInsert' pNode (ForkIsBSTT lIsBST pNode' rIsBST) pn _ =
    gcastWith (proofGtNInsert' pNode rIsBST pn po) $
      gcastWith (proofGtNInsert' pNode rIsBST (Proxy :: Proxy n1) po) $
        gcastWith (proofGtNBalance (ForkIsBSTT lIsBST pNode' rIsBST') pn) Refl
    where
      po = Proxy :: Proxy o
      rIsBST' = proofIsBSTInsert pNode rIsBST

-- | Prove that inserting a node with key @x@ and element value @a@
-- in an `Data.Tree.AVL.Extern.Constructors.AVL` tree preserves the @AVL@ condition.
class ProofIsBalancedInsert (x :: Nat) (a :: Type) (t :: Tree) where
  proofIsBalancedInsert :: Proxy (Node x a) -> IsBalancedT t -> IsBalancedT (Insert x a t)

instance ProofIsBalancedInsert x a 'EmptyTree where
  proofIsBalancedInsert pNode _ = ForkIsBalancedT EmptyIsBalancedT pNode EmptyIsBalancedT

instance
  (o ~ CmpNat x n, ProofIsBalancedInsert' x a ('ForkTree l (Node n a1) r) o) =>
  ProofIsBalancedInsert x a ('ForkTree l (Node n a1) r)
  where
  proofIsBalancedInsert pNode tIsBalanced = proofIsBalancedInsert' pNode tIsBalanced (Proxy :: Proxy o)

-- | Prove that inserting a node with key @x@ and element value @a@
-- in an `Data.Tree.BST.Extern.Constructors.BST` tree preserves the @AVL@ condition, given that the comparison between
-- @x@ and the root key of the tree equals @o@.
-- The @AVL@ condition was already checked when `proofIsBSTInsert` was called before.
-- The @o@ parameter guides the proof.
class ProofIsBalancedInsert' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) where
  proofIsBalancedInsert' :: Proxy (Node x a) -> IsBalancedT t -> Proxy o -> IsBalancedT (Insert' x a t o)

instance ProofIsBalancedInsert' x a ('ForkTree l (Node n a1) r) 'EQ where
  proofIsBalancedInsert' _ (ForkIsBalancedT lIsBalanced _ rIsBalanced) _ = ForkIsBalancedT lIsBalanced pNode rIsBalanced
    where
      pNode = Proxy :: Proxy (Node n a)

instance
  (ProofIsBalancedBalance ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n a1) r)) =>
  ProofIsBalancedInsert' x a ('ForkTree 'EmptyTree (Node n a1) r) 'LT
  where
  proofIsBalancedInsert' pNode (ForkIsBalancedT _ pNode' rIsBalanced) _ =
    proofIsBalancedBalance (ForkIsAlmostBalancedT (ForkIsBalancedT EmptyIsBalancedT pNode EmptyIsBalancedT) pNode' rIsBalanced)

instance
  ( l ~ 'ForkTree ll (Node ln lna) lr,
    o ~ CmpNat x ln,
    ProofIsBalancedInsert' x a l o,
    ProofIsBalancedBalance ('ForkTree (Insert' x a l o) (Node n a1) r)
  ) =>
  ProofIsBalancedInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT
  where
  proofIsBalancedInsert' pNode (ForkIsBalancedT lIsBalanced pNode' rIsBalanced) _ =
    proofIsBalancedBalance $ ForkIsAlmostBalancedT lIsBalanced' pNode' rIsBalanced
    where
      lIsBalanced' = proofIsBalancedInsert pNode lIsBalanced

instance
  (ProofIsBalancedBalance ('ForkTree l (Node n a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree))) =>
  ProofIsBalancedInsert' x a ('ForkTree l (Node n a1) 'EmptyTree) 'GT
  where
  proofIsBalancedInsert' pNode (ForkIsBalancedT lIsBalanced pNode' _) _ =
    proofIsBalancedBalance $ ForkIsAlmostBalancedT lIsBalanced pNode' (ForkIsBalancedT EmptyIsBalancedT pNode EmptyIsBalancedT)

instance
  ( r ~ 'ForkTree rl (Node rn rna) rr,
    o ~ CmpNat x rn,
    ProofIsBalancedInsert' x a r o,
    ProofIsBalancedBalance ('ForkTree l (Node n a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) o))
  ) =>
  ProofIsBalancedInsert' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT
  where
  proofIsBalancedInsert' pNode (ForkIsBalancedT lIsBalanced pNode' rIsBalanced) _ =
    proofIsBalancedBalance $ ForkIsAlmostBalancedT lIsBalanced pNode' rIsBalanced'
    where
      rIsBalanced' = proofIsBalancedInsert pNode rIsBalanced
