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
-- Module      : Data.Tree.BST.Extern.DeleteProofs
-- Description : Proofs for deletion over externalist AVL trees
-- Copyright   : (c) Nicolás Rodríguez, 2021
-- License     : GPL-3
-- Maintainer  : Nicolás Rodríguez
-- Stability   : experimental
-- Portability : POSIX
--
-- Implementation of the necessary proofs to ensure (at compile time) that the
-- deletion algorithm defined in "Data.Tree.AVL.Extern.Delete" respects the key ordering and height balance restrictions.
module Data.Tree.AVL.Extern.DeleteProofs
  ( IsBalancedT (..),
    ProofIsBalancedDelete (proofIsBalancedDelete),
    ProofIsBSTDelete (proofIsBSTDelete),
  )
where

import Data.Proxy (Proxy (Proxy))
import Data.Tree.AVL.Extern.BalanceProofs
  ( ProofGtNBalance (proofGtNBalance),
    ProofIsBSTBalance (proofIsBSTBalance),
    ProofIsBalancedBalance (proofIsBalancedBalance),
    ProofLtNBalance (proofLtNBalance),
  )
import Data.Tree.AVL.Extern.Constructors (IsAlmostBalancedT (ForkIsAlmostBalancedT), IsBalancedT (EmptyIsBalancedT, ForkIsBalancedT))
import Data.Tree.AVL.Extern.Delete
  ( Deletable (Delete),
    Deletable' (Delete'),
    MaxKeyDeletable (MaxKeyDelete),
  )
import Data.Tree.BST.Extern.Constructors (IsBSTT (EmptyIsBSTT, ForkIsBSTT))
import Data.Tree.BST.Extern.Delete (MaxKey, MaxValue)
import Data.Tree.BST.Invariants (GtN, LtN)
import Data.Tree.ITree (Tree (EmptyTree, ForkTree))
import Data.Tree.Node (Node)
import Data.Type.Equality (gcastWith, (:~:) (Refl))
import GHC.TypeNats (CmpNat, Nat)
import Prelude (Bool (True), Ordering (EQ, GT, LT), ($))

-- | Prove that deleting a node with key @x@
-- | in an `Data.Tree.AVL.Extern.Constructors.AVL` tree preserves the @AVL@ restrictions.
class ProofIsBSTDelete (x :: Nat) (t :: Tree) where
  proofIsBSTDelete :: Proxy x -> IsBSTT t -> IsBSTT (Delete x t)

instance ProofIsBSTDelete x 'EmptyTree where
  proofIsBSTDelete _ _ = EmptyIsBSTT

instance
  ( o ~ CmpNat x n,
    ProofIsBSTDelete' x ('ForkTree l (Node n a1) r) o
  ) =>
  ProofIsBSTDelete x ('ForkTree l (Node n a1) r)
  where
  proofIsBSTDelete px tIsBST = proofIsBSTDelete' px tIsBST (Proxy :: Proxy o)

-- | Prove that inserting a node with key @x@ and element value @a@
-- in an `Data.Tree.AVL.Extern.Constructors.AVL` tree preserves the @AVL@ restrictions, given that the comparison between
-- @x@ and the root key of the tree equals @o@.
-- The @AVL@ restrictions were already checked when `Data.Tree.AVL.Extern.InsertProofs.proofIsBSTInsert` was called before.
-- The @o@ parameter guides the proof.
class ProofIsBSTDelete' (x :: Nat) (t :: Tree) (o :: Ordering) where
  proofIsBSTDelete' :: Proxy x -> IsBSTT t -> Proxy o -> IsBSTT (Delete' x t o)

instance ProofIsBSTDelete' x ('ForkTree 'EmptyTree (Node n a1) 'EmptyTree) 'EQ where
  proofIsBSTDelete' _ _ _ = EmptyIsBSTT

instance ProofIsBSTDelete' x ('ForkTree 'EmptyTree (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  proofIsBSTDelete' _ (ForkIsBSTT _ _ rIsBST) _ = rIsBST

instance ProofIsBSTDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) 'EmptyTree) 'EQ where
  proofIsBSTDelete' _ (ForkIsBSTT lIsBST _ _) _ = lIsBST

instance
  ( l ~ 'ForkTree ll (Node ln la) lr,
    r ~ 'ForkTree rl (Node rn ra) rr,
    LtN (MaxKeyDelete l) (MaxKey l) ~ 'True,
    GtN r (MaxKey l) ~ 'True,
    ProofMaxKeyDeleteIsBST l,
    ProofIsBSTBalance ('ForkTree (MaxKeyDelete l) (Node (MaxKey l) (MaxValue l)) r)
  ) =>
  ProofIsBSTDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ
  where
  proofIsBSTDelete' _ (ForkIsBSTT lIsBST _ rIsBST) _ =
    proofIsBSTBalance $ ForkIsBSTT (proofMaxKeyDeleteIsBST lIsBST) pNode rIsBST
    where
      pNode = Proxy :: Proxy (Node (MaxKey l) (MaxValue l))

instance ProofIsBSTDelete' x ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  proofIsBSTDelete' _ tIsBST _ = tIsBST

instance
  ( l ~ 'ForkTree ll (Node ln la) lr,
    o ~ CmpNat x ln,
    CmpNat x n ~ 'LT,
    ProofIsBSTDelete' x l o,
    ProofLtNDelete' x l n o,
    ProofIsBSTBalance ('ForkTree (Delete' x l o) (Node n a1) r)
  ) =>
  ProofIsBSTDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) r) 'LT
  where
  proofIsBSTDelete' px (ForkIsBSTT lIsBST node rIsBST) _ =
    gcastWith (proofLtNDelete' px lIsBST (Proxy :: Proxy n) (Proxy :: Proxy o)) $
      proofIsBSTBalance $ ForkIsBSTT (proofIsBSTDelete px lIsBST) node rIsBST

instance ProofIsBSTDelete' x ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  proofIsBSTDelete' _ tIsBST _ = tIsBST

instance
  ( r ~ 'ForkTree rl (Node rn ra) rr,
    o ~ CmpNat x rn,
    CmpNat x n ~ 'GT,
    ProofIsBSTDelete' x r o,
    ProofGtNDelete' x r n o,
    ProofIsBSTBalance ('ForkTree l (Node n a1) (Delete' x r o))
  ) =>
  ProofIsBSTDelete' x ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'GT
  where
  proofIsBSTDelete' px (ForkIsBSTT lIsBST node rIsBST) _ =
    gcastWith (proofGtNDelete' px rIsBST (Proxy :: Proxy n) (Proxy :: Proxy o)) $
      proofIsBSTBalance $ ForkIsBSTT lIsBST node (proofIsBSTDelete px rIsBST)

-- | Prove that deleting a node with key @x@ (lower than @n@)
-- in a tree @t@ which verifies @LtN t n ~ 'True@ preserves the `LtN` invariant,
-- given that the comparison between @x@ and the root key of the tree equals @o@.
-- The @o@ parameter guides the proof.
class ProofLtNDelete' (x :: Nat) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofLtNDelete' ::
    (CmpNat x n ~ 'LT, LtN t n ~ 'True) =>
    Proxy x ->
    IsBSTT t ->
    Proxy n ->
    Proxy o ->
    LtN (Delete' x t o) n :~: 'True

instance ProofLtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) 'EmptyTree) n 'EQ where
  proofLtNDelete' _ _ _ _ = Refl

instance
  (LtN ('ForkTree rl (Node rn ra) rr) n ~ 'True) =>
  ProofLtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ
  where
  proofLtNDelete' _ _ _ _ = Refl

instance
  (LtN ('ForkTree ll (Node ln la) lr) n ~ 'True) =>
  ProofLtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) 'EmptyTree) n 'EQ
  where
  proofLtNDelete' _ _ _ _ = Refl

instance
  ( l ~ 'ForkTree ll (Node ln la) lr,
    r ~ 'ForkTree rl (Node rn ra) rr,
    LtN l n ~ 'True,
    LtN r n ~ 'True,
    GtN r (MaxKey l) ~ 'True,
    LtN (MaxKeyDelete l) (MaxKey l) ~ 'True,
    ProofLTMaxKey l n,
    ProofLtNMaxKeyDelete l n,
    ProofMaxKeyDeleteIsBST l,
    ProofLtNBalance ('ForkTree (MaxKeyDelete l) (Node (MaxKey l) (MaxValue l)) r) n
  ) =>
  ProofLtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ
  where
  proofLtNDelete' _ (ForkIsBSTT lIsBST _ rIsBST) pn _ =
    gcastWith (proofLtNMaxKeyDelete lIsBST pn) $
      gcastWith (proofLTMaxKey lIsBST pn) $
        gcastWith (proofLtNBalance (ForkIsBSTT lIsBST' pNode' rIsBST) pn) Refl
    where
      lIsBST' = proofMaxKeyDeleteIsBST lIsBST
      pNode' = Proxy :: Proxy (Node (MaxKey l) (MaxValue l))

instance ProofLtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofLtNDelete' _ _ _ _ = Refl

instance
  ( l ~ 'ForkTree ll (Node ln la) lr,
    o ~ CmpNat x ln,
    LtN l n ~ 'True,
    LtN r n ~ 'True,
    CmpNat x n1 ~ 'LT,
    ProofLtNDelete' x l n o,
    ProofIsBSTDelete x l,
    ProofLtNDelete' x l n1 o,
    ProofLtNBalance ('ForkTree (Delete' x l o) (Node n1 a1) r) n
  ) =>
  ProofLtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) r) n 'LT
  where
  proofLtNDelete' px (ForkIsBSTT lIsBST node rIsBST) pn _ =
    gcastWith (proofLtNDelete' px lIsBST pn po) $
      gcastWith (proofLtNDelete' px lIsBST (Proxy :: Proxy n1) po) $
        gcastWith (proofLtNBalance (ForkIsBSTT lIsBST' node rIsBST) pn) Refl
    where
      po = Proxy :: Proxy o
      lIsBST' = proofIsBSTDelete px lIsBST

instance ProofLtNDelete' x ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofLtNDelete' _ _ _ _ = Refl

instance
  ( r ~ 'ForkTree rl (Node rn ra) rr,
    o ~ CmpNat x rn,
    CmpNat x n1 ~ 'GT,
    LtN r n ~ 'True,
    ProofLtNDelete' x r n o,
    ProofGtNDelete' x r n1 o,
    ProofIsBSTDelete x r,
    ProofLtNBalance ('ForkTree l (Node n1 a1) (Delete' x r o)) n
  ) =>
  ProofLtNDelete' x ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'GT
  where
  proofLtNDelete' px (ForkIsBSTT lIsBST node rIsBST) pn _ =
    gcastWith (proofLtNDelete' px rIsBST pn po) $
      gcastWith (proofGtNDelete' px rIsBST (Proxy :: Proxy n1) po) $
        gcastWith (proofLtNBalance (ForkIsBSTT lIsBST node rIsBST') pn) Refl
    where
      po = Proxy :: Proxy o
      rIsBST' = proofIsBSTDelete px rIsBST

-- | Prove that deleting a node with key @x@ (greater than @n@)
-- in a tree @t@ which verifies @GtN t n ~ 'True@ preserves the `GtN` invariant,
-- given that the comparison between @x@ and the root key of the tree equals @o@.
-- The @o@ parameter guides the proof.
class ProofGtNDelete' (x :: Nat) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofGtNDelete' ::
    (CmpNat x n ~ 'GT, GtN t n ~ 'True) =>
    Proxy x ->
    IsBSTT t ->
    Proxy n ->
    Proxy o ->
    GtN (Delete' x t o) n :~: 'True

instance ProofGtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) 'EmptyTree) n 'EQ where
  proofGtNDelete' _ _ _ _ = Refl

instance
  (GtN ('ForkTree rl (Node rn ra) rr) n ~ 'True) =>
  ProofGtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ
  where
  proofGtNDelete' _ _ _ _ = Refl

instance
  (GtN ('ForkTree ll (Node ln la) lr) n ~ 'True) =>
  ProofGtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) 'EmptyTree) n 'EQ
  where
  proofGtNDelete' _ _ _ _ = Refl

instance
  ( l ~ 'ForkTree ll (Node ln la) lr,
    r ~ 'ForkTree rl (Node rn ra) rr,
    MaxKeyDeletable l,
    GtN l n ~ 'True,
    GtN r n ~ 'True,
    GtN r (MaxKey l) ~ 'True,
    LtN (MaxKeyDelete l) (MaxKey l) ~ 'True,
    ProofGTMaxKey l n,
    ProofGtNMaxKeyDelete l n,
    ProofMaxKeyDeleteIsBST l,
    ProofGtNBalance ('ForkTree (MaxKeyDelete l) (Node (MaxKey l) (MaxValue l)) r) n
  ) =>
  ProofGtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ
  where
  proofGtNDelete' _ (ForkIsBSTT lIsBST _ rIsBST) pn _ =
    gcastWith (proofGtNMaxKeyDelete lIsBST pn) $
      gcastWith (proofGTMaxKey lIsBST pn) $
        gcastWith (proofGtNBalance (ForkIsBSTT lIsBST' pNode' rIsBST) pn) Refl
    where
      lIsBST' = proofMaxKeyDeleteIsBST lIsBST
      pNode' = Proxy :: Proxy (Node (MaxKey l) (MaxValue l))

instance ProofGtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofGtNDelete' _ _ _ _ = Refl

instance
  ( l ~ 'ForkTree ll (Node ln la) lr,
    o ~ CmpNat x ln,
    CmpNat x n1 ~ 'LT,
    GtN l n ~ 'True,
    GtN r n ~ 'True,
    ProofLtNDelete' x l n1 o,
    ProofGtNDelete' x l n o,
    ProofIsBSTDelete x l,
    ProofGtNBalance ('ForkTree (Delete' x l o) (Node n1 a1) r) n
  ) =>
  ProofGtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) r) n 'LT
  where
  proofGtNDelete' px (ForkIsBSTT lIsBST node rIsBST) pn _ =
    gcastWith (proofGtNDelete' px lIsBST pn po) $
      gcastWith (proofLtNDelete' px lIsBST (Proxy :: Proxy n1) po) $
        gcastWith (proofGtNBalance (ForkIsBSTT lIsBST' node rIsBST) pn) Refl
    where
      po = Proxy :: Proxy o
      lIsBST' = proofIsBSTDelete px lIsBST

instance ProofGtNDelete' x ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofGtNDelete' _ _ _ _ = Refl

instance
  ( r ~ 'ForkTree rl (Node rn ra) rr,
    o ~ CmpNat x rn,
    CmpNat x n1 ~ 'GT,
    GtN r n ~ 'True,
    ProofGtNDelete' x r n o,
    ProofGtNDelete' x r n1 o,
    ProofIsBSTDelete x r,
    ProofGtNBalance ('ForkTree l (Node n1 a1) (Delete' x r o)) n
  ) =>
  ProofGtNDelete' x ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'GT
  where
  proofGtNDelete' px (ForkIsBSTT lIsBST node rIsBST) pn _ =
    gcastWith (proofGtNDelete' px rIsBST pn po) $
      gcastWith (proofGtNDelete' px rIsBST (Proxy :: Proxy n1) po) $
        gcastWith (proofGtNBalance (ForkIsBSTT lIsBST node rIsBST') pn) Refl
    where
      po = Proxy :: Proxy o
      rIsBST' = proofIsBSTDelete px rIsBST

-- | Prove that deleting the node with maximum key value
-- in a `Data.Tree.BST.Extern.Constructors.BST` @t@ preserves the @BST@ restrictions.
-- This proof is needed for the delete operation.
class ProofMaxKeyDeleteIsBST (t :: Tree) where
  proofMaxKeyDeleteIsBST :: IsBSTT t -> IsBSTT (MaxKeyDelete t)

instance ProofMaxKeyDeleteIsBST 'EmptyTree where
  proofMaxKeyDeleteIsBST _ = EmptyIsBSTT

instance ProofMaxKeyDeleteIsBST ('ForkTree 'EmptyTree (Node n a) 'EmptyTree) where
  proofMaxKeyDeleteIsBST _ = EmptyIsBSTT

instance ProofMaxKeyDeleteIsBST ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) 'EmptyTree) where
  proofMaxKeyDeleteIsBST (ForkIsBSTT lIsBST _ _) = lIsBST

instance
  ( l ~ 'ForkTree ll (Node ln la) lr,
    r ~ 'ForkTree rl (Node rn ra) rr,
    MaxKeyDeletable r,
    ProofGtNMaxKeyDelete r n,
    ProofMaxKeyDeleteIsBST r,
    ProofIsBSTBalance ('ForkTree l (Node n a) (MaxKeyDelete r))
  ) =>
  ProofMaxKeyDeleteIsBST ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) ('ForkTree rl (Node rn ra) rr))
  where
  proofMaxKeyDeleteIsBST (ForkIsBSTT lIsBST node rIsBST) =
    gcastWith (proofGtNMaxKeyDelete rIsBST (Proxy :: Proxy n)) $
      proofIsBSTBalance (ForkIsBSTT lIsBST node rIsBST')
    where
      rIsBST' = proofMaxKeyDeleteIsBST rIsBST

-- | Prove that in a tree @t@ which verifies that @GtN t n ~ 'True@,
-- the maximum key of @t@ is also greater than @n@.
-- This proof is needed for the `Data.Tree.AVL.Extern.Delete.delete` operation.
class ProofGTMaxKey (t :: Tree) (n :: Nat) where
  proofGTMaxKey ::
    (GtN t n ~ 'True) =>
    IsBSTT t ->
    Proxy n ->
    CmpNat (MaxKey t) n :~: 'GT

instance
  (CmpNat n1 n ~ 'GT) =>
  ProofGTMaxKey ('ForkTree l (Node n1 a) 'EmptyTree) n
  where
  proofGTMaxKey _ _ = Refl

instance
  ( r ~ 'ForkTree rl (Node rn ra) rr,
    GtN r n ~ 'True,
    ProofGTMaxKey r n
  ) =>
  ProofGTMaxKey ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n
  where
  proofGTMaxKey (ForkIsBSTT _ _ rIsBST) pn =
    gcastWith (proofGTMaxKey rIsBST pn) Refl

-- | Prove that in a tree @t@ which verifies that @GtN t n ~ 'True@,
-- the tree resulting from the removal of the maximum key of @t@ preserves the `GtN` invariant.
-- This proof is needed for the `Data.Tree.AVL.Extern.Delete.delete` operation.
class ProofGtNMaxKeyDelete (t :: Tree) (n :: Nat) where
  proofGtNMaxKeyDelete ::
    (MaxKeyDeletable t, GtN t n ~ 'True) =>
    IsBSTT t ->
    Proxy n ->
    GtN (MaxKeyDelete t) n :~: 'True

instance
  (GtN l n ~ 'True) =>
  ProofGtNMaxKeyDelete ('ForkTree l (Node n1 a) 'EmptyTree) n
  where
  proofGtNMaxKeyDelete _ _ = Refl

instance
  ( r ~ 'ForkTree rl (Node rn ra) rr,
    GtN l n ~ 'True,
    GtN r n ~ 'True,
    MaxKeyDeletable r,
    ProofGtNMaxKeyDelete r n,
    ProofGtNMaxKeyDelete r n1,
    ProofMaxKeyDeleteIsBST r,
    ProofGtNBalance ('ForkTree l (Node n1 a) (MaxKeyDelete r)) n
  ) =>
  ProofGtNMaxKeyDelete ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n
  where
  proofGtNMaxKeyDelete (ForkIsBSTT lIsBST node rIsBST) pn =
    gcastWith (proofGtNMaxKeyDelete rIsBST pn) $
      gcastWith (proofGtNMaxKeyDelete rIsBST (Proxy :: Proxy n1)) $
        gcastWith (proofGtNBalance (ForkIsBSTT lIsBST node rIsBST') pn) Refl
    where
      rIsBST' = proofMaxKeyDeleteIsBST rIsBST

-- | Prove that in a tree @t@ which verifies that @LtN t n ~ 'True@,
-- the maximum key of @t@ is also less than @n@.
-- This proof is needed for the `Data.Tree.AVL.Extern.Delete.delete` operation.
class ProofLTMaxKey (t :: Tree) (n :: Nat) where
  proofLTMaxKey ::
    (LtN t n ~ 'True) =>
    IsBSTT t ->
    Proxy n ->
    CmpNat (MaxKey t) n :~: 'LT

instance
  (CmpNat n1 n ~ 'LT) =>
  ProofLTMaxKey ('ForkTree l (Node n1 a) 'EmptyTree) n
  where
  proofLTMaxKey _ _ = Refl

instance
  ( r ~ 'ForkTree rl (Node rn ra) rr,
    LtN r n ~ 'True,
    ProofLTMaxKey r n
  ) =>
  ProofLTMaxKey ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n
  where
  proofLTMaxKey (ForkIsBSTT _ _ rIsBST) pn =
    gcastWith (proofLTMaxKey rIsBST pn) Refl

-- | Prove that in a tree @t@ which verifies that @LtN t n ~ 'True@,
-- the tree resulting from the removal of the maximum key of @t@ preserves the `LtN` invariant.
-- This proof is needed for the `Data.Tree.AVL.Extern.Delete.delete` operation.
class ProofLtNMaxKeyDelete (t :: Tree) (n :: Nat) where
  proofLtNMaxKeyDelete ::
    (LtN t n ~ 'True) =>
    IsBSTT t ->
    Proxy n ->
    LtN (MaxKeyDelete t) n :~: 'True

instance
  (LtN l n ~ 'True) =>
  ProofLtNMaxKeyDelete ('ForkTree l (Node n1 a) 'EmptyTree) n
  where
  proofLtNMaxKeyDelete _ _ = Refl

instance
  ( r ~ 'ForkTree rl (Node rn ra) rr,
    LtN r n ~ 'True,
    MaxKeyDeletable r,
    ProofGtNMaxKeyDelete r n1,
    ProofMaxKeyDeleteIsBST r,
    ProofLtNMaxKeyDelete r n,
    ProofLtNBalance ('ForkTree l (Node n1 a) (MaxKeyDelete r)) n
  ) =>
  ProofLtNMaxKeyDelete ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n
  where
  proofLtNMaxKeyDelete (ForkIsBSTT lIsBST node rIsBST) pn =
    gcastWith (proofLtNMaxKeyDelete rIsBST pn) $
      gcastWith (proofGtNMaxKeyDelete rIsBST (Proxy :: Proxy n1)) $
        gcastWith (proofLtNBalance (ForkIsBSTT lIsBST node rIsBST') pn) Refl
    where
      rIsBST' = proofMaxKeyDeleteIsBST rIsBST

-- | Prove that deleting the node with maximum key value
-- in an `Data.Tree.AVL.Extern.Constructors.AVL` @t@ preserves the @AVL@ restrictions.
-- This proof is needed for the `Data.Tree.AVL.Extern.Delete.delete` operation.
class ProofMaxKeyDeleteIsBalanced (t :: Tree) where
  proofMaxKeyDeleteIsBalanced :: IsBalancedT t -> IsBalancedT (MaxKeyDelete t)

instance ProofMaxKeyDeleteIsBalanced 'EmptyTree where
  proofMaxKeyDeleteIsBalanced _ = EmptyIsBalancedT

instance ProofMaxKeyDeleteIsBalanced ('ForkTree 'EmptyTree (Node n a) 'EmptyTree) where
  proofMaxKeyDeleteIsBalanced _ = EmptyIsBalancedT

instance ProofMaxKeyDeleteIsBalanced ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) 'EmptyTree) where
  proofMaxKeyDeleteIsBalanced (ForkIsBalancedT lIsBalanced _ _) = lIsBalanced

instance
  ( l ~ 'ForkTree ll (Node ln la) lr,
    r ~ 'ForkTree rl (Node rn ra) rr,
    ProofMaxKeyDeleteIsBalanced r,
    ProofIsBalancedBalance ('ForkTree l (Node n a) (MaxKeyDelete r))
  ) =>
  ProofMaxKeyDeleteIsBalanced ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) ('ForkTree rl (Node rn ra) rr))
  where
  proofMaxKeyDeleteIsBalanced (ForkIsBalancedT lIsBalanced node rIsBalanced) =
    proofIsBalancedBalance $ ForkIsAlmostBalancedT lIsBalanced node (proofMaxKeyDeleteIsBalanced rIsBalanced)

-- | Prove that deleting a node with key @x@
-- in an `Data.Tree.AVL.Extern.Constructors.AVL` tree preserves the @AVL@ condition.
class ProofIsBalancedDelete (x :: Nat) (t :: Tree) where
  proofIsBalancedDelete :: Proxy x -> IsBalancedT t -> IsBalancedT (Delete x t)

instance ProofIsBalancedDelete x 'EmptyTree where
  proofIsBalancedDelete _ _ = EmptyIsBalancedT

instance
  ( o ~ CmpNat x n,
    ProofIsBalancedDelete' x ('ForkTree l (Node n a1) r) o
  ) =>
  ProofIsBalancedDelete x ('ForkTree l (Node n a1) r)
  where
  proofIsBalancedDelete px tIsBalanced = proofIsBalancedDelete' px tIsBalanced (Proxy :: Proxy o)

-- | Prove that deleting a node with key @x@
-- in an `Data.Tree.AVL.Extern.Constructors.AVL` tree preserves the @AVL@ condition, given that the comparison between
-- @x@ and the root key of the tree equals @o@.
-- The @AVL@ restrictions were already checked when `proofIsBSTDelete` was called before.
-- The @o@ parameter guides the proof.
class ProofIsBalancedDelete' (x :: Nat) (t :: Tree) (o :: Ordering) where
  proofIsBalancedDelete' :: Proxy x -> IsBalancedT t -> Proxy o -> IsBalancedT (Delete' x t o)

instance ProofIsBalancedDelete' x ('ForkTree 'EmptyTree (Node n a1) 'EmptyTree) 'EQ where
  proofIsBalancedDelete' _ _ _ = EmptyIsBalancedT

instance ProofIsBalancedDelete' x ('ForkTree 'EmptyTree (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  proofIsBalancedDelete' _ (ForkIsBalancedT _ _ rIsBalanced) _ = rIsBalanced

instance ProofIsBalancedDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) 'EmptyTree) 'EQ where
  proofIsBalancedDelete' _ (ForkIsBalancedT lIsBalanced _ _) _ = lIsBalanced

instance
  ( l ~ 'ForkTree ll (Node ln la) lr,
    r ~ 'ForkTree rl (Node rn ra) rr,
    ProofMaxKeyDeleteIsBalanced l,
    ProofIsBalancedBalance ('ForkTree (MaxKeyDelete l) (Node (MaxKey l) (MaxValue l)) r)
  ) =>
  ProofIsBalancedDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ
  where
  proofIsBalancedDelete' _ (ForkIsBalancedT lIsBalanced _ rIsBalanced) _ =
    proofIsBalancedBalance (ForkIsAlmostBalancedT lIsBalanced' pNode rIsBalanced)
    where
      lIsBalanced' = proofMaxKeyDeleteIsBalanced lIsBalanced
      pNode = Proxy :: Proxy (Node (MaxKey l) (MaxValue l))

instance ProofIsBalancedDelete' x ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  proofIsBalancedDelete' _ tIsBalanced _ = tIsBalanced

instance
  ( l ~ 'ForkTree ll (Node ln la) lr,
    ProofIsBalancedDelete x l,
    ProofIsBalancedBalance ('ForkTree (Delete' x l (CmpNat x ln)) (Node n a1) r)
  ) =>
  ProofIsBalancedDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) r) 'LT
  where
  proofIsBalancedDelete' px (ForkIsBalancedT lIsBalanced node rIsBalanced) _ =
    proofIsBalancedBalance (ForkIsAlmostBalancedT lIsBalanced' node rIsBalanced)
    where
      lIsBalanced' = proofIsBalancedDelete px lIsBalanced

instance ProofIsBalancedDelete' x ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  proofIsBalancedDelete' _ tIsBalanced _ = tIsBalanced

instance
  ( r ~ 'ForkTree rl (Node rn ra) rr,
    ProofIsBalancedDelete x r,
    ProofIsBalancedBalance ('ForkTree l (Node n a1) (Delete' x r (CmpNat x rn)))
  ) =>
  ProofIsBalancedDelete' x ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'GT
  where
  proofIsBalancedDelete' px (ForkIsBalancedT lIsBalanced node rIsBalanced) _ =
    proofIsBalancedBalance (ForkIsAlmostBalancedT lIsBalanced node rIsBalanced')
    where
      rIsBalanced' = proofIsBalancedDelete px rIsBalanced
