{-# OPTIONS_HADDOCK ignore-exports #-}

{-|
Module      : Data.Tree.AVL.Intern.Delete
Description : Deletion algorithm (with proofs) over internalist AVL trees
Copyright   : (c) Nicolás Rodríguez, 2021
License     : GPL-3
Maintainer  : Nicolás Rodríguez
Stability   : experimental
Portability : POSIX

Implementation of the deletion algorithm over internalist AVL trees,
along with the necessary proofs to ensure (at compile time) that the
key ordering and height balancing still holds.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# LANGUAGE Safe                  #-}

module Data.Tree.AVL.Intern.Delete (
  Deletable(Delete,delete)
) where

import           Data.Kind                        (Type)
import           Data.Proxy                       (Proxy (Proxy))
import           Data.Tree.AVL.Intern.Balance     (Balanceable (Balance, balance),
                                                   ProofGtNBalance (proofGtNBalance),
                                                   ProofLtNBalance (proofLtNBalance))
import           Data.Tree.AVL.Intern.Constructors (AVL (EmptyAVL, ForkAVL),
                                                   AlmostAVL (AlmostAVL))
import           Data.Tree.BST.Invariants         (GtN, LtN)
import           Data.Tree.ITree                  (Tree (EmptyTree, ForkTree))
import           Data.Tree.Node                   (Node, mkNode, getValue)
import           Data.Type.Equality               ((:~:) (Refl), gcastWith)
import           GHC.TypeLits                     (CmpNat, Nat)
import           Prelude                          (Bool (True),
                                                   Ordering (EQ, GT, LT), Show,
                                                   ($))


-- | Prove that deleting a node with key @x@ (lower than @n@)
-- in a tree @t@ which verifies @LtN t n ~ 'True@ preserves the `LtN` invariant,
-- given that the comparison between @x@ and the root key of the tree equals @o@.
-- The @o@ parameter guides the proof.
class ProofLtNDelete' (x :: Nat) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofLtNDelete' :: (CmpNat x n ~ 'LT, LtN t n ~ 'True) =>
    Proxy x -> AVL t -> Proxy n -> Proxy o -> LtN (Delete' x t o) n :~: 'True
instance ProofLtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) 'EmptyTree) n 'EQ where
  proofLtNDelete' _ _ _ _ = Refl
instance (LtN ('ForkTree rl (Node rn ra) rr) n ~ 'True) =>
  ProofLtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ where
  proofLtNDelete' _ _ _ _ = Refl
instance (LtN ('ForkTree ll (Node ln la) lr) n ~ 'True) =>
  ProofLtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) 'EmptyTree) n 'EQ where
  proofLtNDelete' _ _ _ _ = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, r ~ 'ForkTree rl (Node rn ra) rr,
  LtN l n ~ 'True, LtN r n ~ 'True, GtN r (MaxKey l) ~ 'True, LtN (MaxKeyDelete l) (MaxKey l) ~ 'True,
  Show (MaxValue l), Maxable l, MaxKeyDeletable l,
  ProofLTMaxKey l n, ProofLtNMaxKeyDelete l n, ProofLtNBalance ('ForkTree (MaxKeyDelete l) (Node (MaxKey l) (MaxValue l)) r) n) =>
  ProofLtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ where
  proofLtNDelete' _ (ForkAVL l _ r) pn _ =
    gcastWith (proofLtNMaxKeyDelete l pn) $
    gcastWith (proofLTMaxKey l pn) $
    gcastWith (proofLtNBalance (AlmostAVL l' node' r) pn) Refl
      where
        l' = maxKeyDelete l
        node' = mkNode (Proxy::Proxy (MaxKey l)) (maxValue l)
instance ProofLtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofLtNDelete' _ _ _ _ = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, o ~ CmpNat x ln,
  CmpNat x n1 ~ 'LT, LtN l n ~ 'True,
  Deletable x l,
  ProofLtNDelete' x l n o, ProofLtNBalance ('ForkTree (Delete' x l o) (Node n1 a1) r) n, ProofLtNDelete' x l n1 o) =>
  ProofLtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) r) n 'LT where
  proofLtNDelete' px (ForkAVL l node r) pn _ =
    gcastWith (proofLtNDelete' px l pn po) $
    gcastWith (proofLtNDelete' px l (Proxy::Proxy n1) po) $
    gcastWith (proofLtNBalance (AlmostAVL l' node r) pn) Refl
      where
        po = Proxy::Proxy o
        l' = delete px l
instance ProofLtNDelete' x ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofLtNDelete' _ _ _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, o ~ CmpNat x rn,
  CmpNat x n1 ~ 'GT, LtN r n ~ 'True,
  Deletable x r,
  ProofLtNDelete' x r n o, ProofLtNBalance ('ForkTree l (Node n1 a1) (Delete' x r o)) n, ProofGtNDelete' x r n1 o) =>
  ProofLtNDelete' x ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'GT where
  proofLtNDelete' px (ForkAVL l node r) pn _ =
    gcastWith (proofLtNDelete' px r pn po) $
    gcastWith (proofGtNDelete' px r (Proxy::Proxy n1) po) $
    gcastWith (proofLtNBalance (AlmostAVL l node r') pn) Refl
      where
        po = Proxy::Proxy o
        r' = delete px r


-- | Prove that deleting a node with key @x@ (greater than @n@)
-- in a tree @t@ which verifies @GtN t n ~ 'True@ preserves the `GtN` invariant,
-- given that the comparison between @x@ and the root key of the tree equals @o@.
-- The @o@ parameter guides the proof.
class ProofGtNDelete' (x :: Nat) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofGtNDelete' :: (CmpNat x n ~ 'GT, GtN t n ~ 'True) =>
    Proxy x -> AVL t -> Proxy n -> Proxy o -> GtN (Delete' x t o) n :~: 'True
instance ProofGtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) 'EmptyTree) n 'EQ where
  proofGtNDelete' _ _ _ _ = Refl
instance (GtN ('ForkTree rl (Node rn ra) rr) n ~ 'True) =>
  ProofGtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ where
  proofGtNDelete' _ _ _ _ = Refl
instance (GtN ('ForkTree ll (Node ln la) lr) n ~ 'True) =>
  ProofGtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) 'EmptyTree) n 'EQ where
  proofGtNDelete' _ _ _ _ = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, r ~ 'ForkTree rl (Node rn ra) rr,
  GtN l n ~ 'True, GtN r n ~ 'True, GtN r (MaxKey l) ~ 'True, LtN (MaxKeyDelete l) (MaxKey l) ~ 'True,
  Show (MaxValue l), Maxable l, MaxKeyDeletable l,
  ProofGTMaxKey l n, ProofGtNMaxKeyDelete l n, ProofGtNBalance ('ForkTree (MaxKeyDelete l) (Node (MaxKey l) (MaxValue l)) r) n) =>
  ProofGtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ where
  proofGtNDelete' _ (ForkAVL l _ r) pn _ =
    gcastWith (proofGtNMaxKeyDelete l pn) $
    gcastWith (proofGTMaxKey l pn) $
    gcastWith (proofGtNBalance (AlmostAVL l' node' r) pn) Refl
      where
        l' = maxKeyDelete l
        node' = mkNode (Proxy::Proxy (MaxKey l)) (maxValue l)
instance ProofGtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofGtNDelete' _ _ _ _ = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, o ~ CmpNat x ln,
  CmpNat x n1 ~ 'LT, GtN l n ~ 'True,
  Deletable x l,
  ProofGtNDelete' x l n o, ProofGtNBalance ('ForkTree (Delete' x l o) (Node n1 a1) r) n, ProofLtNDelete' x l n1 o) =>
  ProofGtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) r) n 'LT where
  proofGtNDelete' px (ForkAVL l node r) pn _ =
    gcastWith (proofGtNDelete' px l pn po) $
    gcastWith (proofLtNDelete' px l (Proxy::Proxy n1) po) $
    gcastWith (proofGtNBalance (AlmostAVL l' node r) pn) Refl
      where
        po = Proxy::Proxy o
        l' = delete px l
instance ProofGtNDelete' x ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofGtNDelete' _ _ _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, o ~ CmpNat x rn,
  CmpNat x n1 ~ 'GT, GtN r n ~ 'True,
  Deletable x r,
  ProofGtNDelete' x r n o, ProofGtNBalance ('ForkTree l (Node n1 a1) (Delete' x r o)) n, ProofGtNDelete' x r n1 o) =>
  ProofGtNDelete' x ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'GT where
  proofGtNDelete' px (ForkAVL l node r) pn _ =
    gcastWith (proofGtNDelete' px r pn po) $
    gcastWith (proofGtNDelete' px r (Proxy::Proxy n1) po) $
    gcastWith (proofGtNBalance (AlmostAVL l node r') pn) Refl
      where
        po = Proxy::Proxy o
        r' = delete px r


-- | Prove that in a tree @t@ which verifies that @GtN t n ~ 'True@,
-- the maximum key of @t@ is also greater than @n@.
-- This proof is needed for the delete operation.
class ProofGTMaxKey (t :: Tree) (n :: Nat) where
  proofGTMaxKey :: (GtN t n ~ 'True) =>
    AVL t -> Proxy n -> CmpNat (MaxKey t) n :~: 'GT
instance (CmpNat n1 n ~ 'GT) =>
  ProofGTMaxKey ('ForkTree l (Node n1 a) 'EmptyTree) n where
  proofGTMaxKey _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr,
  GtN r n ~ 'True,
  ProofGTMaxKey r n) =>
  ProofGTMaxKey ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofGTMaxKey (ForkAVL _ _ r) pn =
    gcastWith (proofGTMaxKey r pn) Refl

-- | Prove that in a tree @t@ which verifies that @GtN t n ~ 'True@,
-- the tree resulting from the removal of the maximum key of @t@ preserves the `GtN` invariant.
-- This proof is needed for the `delete` operation.
class ProofGtNMaxKeyDelete (t :: Tree) (n :: Nat) where
  proofGtNMaxKeyDelete :: (GtN t n ~ 'True) =>
    AVL t -> Proxy n -> GtN (MaxKeyDelete t) n :~: 'True
instance (GtN l n ~ 'True) =>
  ProofGtNMaxKeyDelete ('ForkTree l (Node n1 a) 'EmptyTree) n where
  proofGtNMaxKeyDelete _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr,
  GtN r n ~ 'True,
  MaxKeyDeletable r,
  ProofGtNMaxKeyDelete r n, ProofGtNBalance ('ForkTree l (Node n1 a) (MaxKeyDelete r)) n, ProofGtNMaxKeyDelete r n1) =>
  ProofGtNMaxKeyDelete ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofGtNMaxKeyDelete (ForkAVL l node r) pn =
    gcastWith (proofGtNMaxKeyDelete r pn) $
    gcastWith (proofGtNMaxKeyDelete r (Proxy::Proxy n1)) $
    gcastWith (proofGtNBalance (AlmostAVL l node r') pn) Refl
      where
        r' = maxKeyDelete r

-- | Prove that in a tree @t@ which verifies that @LtN t n ~ 'True@,
-- the maximum key of @t@ is also less than @n@.
-- This proof is needed for the `delete` operation.
class ProofLTMaxKey (t :: Tree) (n :: Nat) where
  proofLTMaxKey :: (LtN t n ~ 'True) =>
    AVL t -> Proxy n -> CmpNat (MaxKey t) n :~: 'LT
instance (CmpNat n1 n ~ 'LT) =>
  ProofLTMaxKey ('ForkTree l (Node n1 a) 'EmptyTree) n where
  proofLTMaxKey _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr,
  LtN r n ~ 'True,
  ProofLTMaxKey r n) =>
  ProofLTMaxKey ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofLTMaxKey (ForkAVL _ _ r) pn =
    gcastWith (proofLTMaxKey r pn) Refl

-- | Prove that in a tree @t@ which verifies that @LtN t n ~ 'True@,
-- the tree resulting from the removal of the maximum key of @t@ preserves the `LtN` invariant.
-- This proof is needed for the `delete` operation.
class ProofLtNMaxKeyDelete (t :: Tree) (n :: Nat) where
  proofLtNMaxKeyDelete :: (LtN t n ~ 'True) =>
    AVL t -> Proxy n -> LtN (MaxKeyDelete t) n :~: 'True
instance (LtN l n ~ 'True) =>
  ProofLtNMaxKeyDelete ('ForkTree l (Node n1 a) 'EmptyTree) n where
  proofLtNMaxKeyDelete _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr,
  LtN r n ~ 'True, LtN r n1 ~ 'True,
  MaxKeyDeletable r,
  ProofLtNMaxKeyDelete r n, ProofGtNMaxKeyDelete r n1, ProofLtNBalance ('ForkTree l (Node n1 a) (MaxKeyDelete r)) n) =>
  ProofLtNMaxKeyDelete ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofLtNMaxKeyDelete (ForkAVL l node r) pn =
    gcastWith (proofLtNMaxKeyDelete r pn) $
    gcastWith (proofGtNMaxKeyDelete r (Proxy::Proxy n1)) $
    gcastWith (proofLtNBalance (AlmostAVL l node r') pn) Refl
      where
        r' = maxKeyDelete r


-- | This type class provides the functionality to delete the node with maximum key value
-- in an `AVL` @t@.
-- The deletion is defined at the value level and the type level.
class MaxKeyDeletable (t :: Tree) where
  type MaxKeyDelete (t :: Tree) :: Tree
  maxKeyDelete :: (t ~ 'ForkTree l (Node n a1) r) =>
    AVL t -> AVL (MaxKeyDelete t)
instance MaxKeyDeletable ('ForkTree l (Node n a1) 'EmptyTree) where
  type MaxKeyDelete ('ForkTree l (Node n a1) 'EmptyTree) = l
  maxKeyDelete (ForkAVL l _ _) = l
instance (r ~ 'ForkTree rl (Node rn ra) rr,
  MaxKeyDeletable r, Balanceable ('ForkTree l (Node n a1) (MaxKeyDelete r)),
  ProofGtNMaxKeyDelete r n) =>
  MaxKeyDeletable ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) where
  type MaxKeyDelete ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) =
    Balance ('ForkTree l (Node n a1) (MaxKeyDelete ('ForkTree rl (Node rn ra) rr)))
  maxKeyDelete (ForkAVL l node r) =
    gcastWith (proofGtNMaxKeyDelete r (Proxy::Proxy n)) $
    balance $ AlmostAVL l node (maxKeyDelete r)

-- | This type class provides the functionality to get the key, type and value of the node with maximum key value
-- in an `AVL` @t@.
-- The lookup is defined at the value level and the type level.
-- Since the keys are only kept at the type level, there's no value level getter of the maximum key.
class Maxable (t :: Tree) where
  type MaxKey (t :: Tree) :: Nat
  type MaxValue (t :: Tree) :: Type
  maxValue :: (t ~ 'ForkTree l (Node n a1) r) =>
    AVL t -> MaxValue t
instance Maxable ('ForkTree l (Node n a1) 'EmptyTree) where
  type MaxKey ('ForkTree l (Node n a1) 'EmptyTree) = n
  type MaxValue ('ForkTree l (Node n a1) 'EmptyTree) = a1
  maxValue (ForkAVL _ node _) = getValue node
instance (Maxable ('ForkTree rl (Node rn ra) rr)) =>
  Maxable ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) where
  type MaxKey ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) = MaxKey ('ForkTree rl (Node rn ra) rr)
  type MaxValue ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) = MaxValue ('ForkTree rl (Node rn ra) rr)
  maxValue (ForkAVL _ _ r) = maxValue r


-- | This type class provides the functionality to delete the node with key @x@
-- in an `AVL` @t@.
-- The deletion is defined at the value level and the type level,
-- and both return `AVL` trees.
class Deletable (x :: Nat) (t :: Tree) where
  type Delete (x :: Nat) (t :: Tree) :: Tree
  delete :: Proxy x -> AVL t -> AVL (Delete x t)
instance Deletable x 'EmptyTree where
  type Delete x 'EmptyTree = 'EmptyTree
  delete _ _ = EmptyAVL
instance (o ~ CmpNat x n,
  Deletable' x ('ForkTree l (Node n a1) r) o) =>
  Deletable x ('ForkTree l (Node n a1) r) where
  type Delete x ('ForkTree l (Node n a1) r) = Delete' x ('ForkTree l (Node n a1) r) (CmpNat x n)
  delete px t = delete' px t (Proxy::Proxy o)

-- | This type class provides the functionality to delete a node with key @x@
-- in a non empty `AVL` @t@.
-- It's only used by the 'Deletable' class and it has one extra parameter @o@,
-- which is the type level comparison of @x@ with the key value of the root node.
-- The @o@ parameter guides the insertion.
class Deletable' (x :: Nat) (t :: Tree) (o :: Ordering) where
  type Delete' (x :: Nat) (t :: Tree) (o :: Ordering) :: Tree
  delete' :: Proxy x -> AVL t -> Proxy o -> AVL (Delete' x t o)
instance Deletable' x ('ForkTree 'EmptyTree (Node n a1) 'EmptyTree) 'EQ where
  type Delete' x ('ForkTree 'EmptyTree (Node n a1) 'EmptyTree) 'EQ = 'EmptyTree
  delete' _ _ _ = EmptyAVL
instance Deletable' x ('ForkTree 'EmptyTree (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  type Delete' x ('ForkTree 'EmptyTree (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ = ('ForkTree rl (Node rn ra) rr)
  delete' _ (ForkAVL _ _ r) _ = r
instance Deletable' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) 'EmptyTree) 'EQ where
  type Delete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) 'EmptyTree) 'EQ = ('ForkTree ll (Node ln la) lr)
  delete' _ (ForkAVL l _ _) _ = l
instance (l ~ 'ForkTree ll (Node ln la) lr, r ~ 'ForkTree rl (Node rn ra) rr,
  LtN (MaxKeyDelete l) (MaxKey l) ~ 'True, GtN r (MaxKey l) ~ 'True,
  Show (MaxValue l), MaxKeyDeletable l, Maxable l, Balanceable ('ForkTree (MaxKeyDelete l) (Node (MaxKey l) (MaxValue l)) r)) =>
  Deletable' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  type Delete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ =
    Balance ('ForkTree (MaxKeyDelete ('ForkTree ll (Node ln la) lr)) (Node (MaxKey ('ForkTree ll (Node ln la) lr)) (MaxValue ('ForkTree ll (Node ln la) lr))) ('ForkTree rl (Node rn ra) rr))
  delete' _ (ForkAVL l _ r) _ =
    balance $ AlmostAVL (maxKeyDelete l) node r
      where
        node = mkNode (Proxy::Proxy (MaxKey l)) (maxValue l)
instance Deletable' x ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  type Delete' x ('ForkTree 'EmptyTree (Node n a1) r) 'LT = ('ForkTree 'EmptyTree (Node n a1) r)
  delete' _ t _ = t
instance (l ~ 'ForkTree ll (Node ln la) lr, o ~ CmpNat x ln,
  CmpNat x n ~ 'LT,
  Deletable' x l o, Balanceable ('ForkTree (Delete' x l o) (Node n a1) r),
  ProofLtNDelete' x l n o) =>
  Deletable' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) r) 'LT where
  type Delete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) r) 'LT =
    Balance ('ForkTree (Delete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln)) (Node n a1) r)
  delete' px (ForkAVL l node r) _ =
    gcastWith (proofLtNDelete' px l (Proxy::Proxy n) po) $
    balance $ AlmostAVL (delete' px l po) node r
      where
        po = Proxy::Proxy o
instance Deletable' x ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  type Delete' x ('ForkTree l (Node n a1) 'EmptyTree) 'GT = ('ForkTree l (Node n a1) 'EmptyTree)
  delete' _ t _ = t
instance (r ~ 'ForkTree rl (Node rn ra) rr, o ~ CmpNat x rn,
  CmpNat x n ~ 'GT,
  Deletable' x r o, Balanceable ('ForkTree l (Node n a1) (Delete' x r o)),
  ProofGtNDelete' x r n o) =>
  Deletable' x ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'GT where
  type Delete' x ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'GT =
    Balance ('ForkTree l (Node n a1) (Delete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn)))
  delete' px (ForkAVL l node r) _ =
      gcastWith (proofGtNDelete' px r (Proxy::Proxy n) po) $
      balance $ AlmostAVL l node (delete' px r po)
        where
          po = Proxy::Proxy o
