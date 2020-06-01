{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Tree.BST.Intern.Delete (
  Deletable(Delete, delete)
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


-- | This class provides the functionality to delete the node with maximum key value
-- | in a BST 't'.
-- | The deletion is defined at the value level and the type level.
class MaxKeyDeletable (t :: Tree) where
  type MaxKeyDelete (t :: Tree) :: Tree
  maxKeyDelete :: (t ~ 'ForkTree l (Node n a1) r) =>
    BST t -> BST (MaxKeyDelete t)
instance MaxKeyDeletable ('ForkTree l (Node n a1) 'EmptyTree) where
  type MaxKeyDelete ('ForkTree l (Node n a1) 'EmptyTree) = l
  maxKeyDelete (ForkBST l (Node _) EmptyBST) = l
instance (r ~ 'ForkTree rl (Node rn ra) rr, MaxKeyDeletable r,
  ProofGtNMaxKeyDelete r n) =>
  MaxKeyDeletable ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) where
  type MaxKeyDelete ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) =
    ('ForkTree l (Node n a1) (MaxKeyDelete ('ForkTree rl (Node rn ra) rr)))
  maxKeyDelete (ForkBST l node r@ForkBST{}) =
    gcastWith (proofGtNMaxKeyDelete r (Proxy::Proxy n)) $
      ForkBST l node (maxKeyDelete r)


-- | This class provides the functionality to get the key, type and value of the node
-- | with maximum key value in a BST 't'.
-- | The lookup is defined at the value level and the type level.
-- | Since the keys are only kept at the type level,
-- | there's no value level getter of the maximum key.
class Maxable (t :: Tree) where
  type MaxKey (t :: Tree) :: Nat
  type MaxValue (t :: Tree) :: Type
  maxValue :: (t ~ 'ForkTree l (Node n a1) r, a ~ MaxValue t) =>
    BST t -> a
instance Maxable ('ForkTree l (Node n a1) 'EmptyTree) where
  type MaxKey ('ForkTree l (Node n a1) 'EmptyTree) = n
  type MaxValue ('ForkTree l (Node n a1) 'EmptyTree) = a1
  maxValue (ForkBST _ (Node a1) EmptyBST) = a1
instance Maxable ('ForkTree rl (Node rn ra) rr) =>
  Maxable ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) where
  type MaxKey ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) =
    MaxKey ('ForkTree rl (Node rn ra) rr)
  type MaxValue ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) =
    MaxValue ('ForkTree rl (Node rn ra) rr)
  maxValue (ForkBST _ (Node _) r@ForkBST{}) = maxValue r


-- | This class provides the functionality to delete a node with key 'x'
-- | in a BST 't'.
-- | The deletion is defined at the value level and the type level.
-- | The returned tree verifies the BST invariant.
class Deletable (x :: Nat) (t :: Tree) where
  type Delete (x :: Nat) (t :: Tree) :: Tree
  delete :: Proxy x -> BST t -> BST (Delete x t)
instance Deletable x 'EmptyTree where
  type Delete x 'EmptyTree = 'EmptyTree
  delete _ EmptyBST = EmptyBST
instance (Deletable' x ('ForkTree l (Node n a1) r) (CmpNat x n)) =>
  Deletable x ('ForkTree l (Node n a1) r) where
  type Delete x ('ForkTree l (Node n a1) r) = Delete' x ('ForkTree l (Node n a1) r) (CmpNat x n)
  delete px t = delete' px t (Proxy::Proxy (CmpNat x n))

-- | This class provides the functionality to delete a node with key 'x'
-- | in a non empty BST 't'.
-- | It's only used by the 'Deletable' class and it has one extra parameter 'o',
-- | which is the type level comparison of 'x' with the key value of the root node.
-- | The 'o' parameter guides the insertion.
class Deletable' (x :: Nat) (t :: Tree) (o :: Ordering) where
  type Delete' (x :: Nat) (t :: Tree) (o :: Ordering) :: Tree
  delete' :: Proxy x -> BST t -> Proxy o -> BST (Delete' x t o)
instance Deletable' x ('ForkTree 'EmptyTree (Node n a1) 'EmptyTree) 'EQ where
  type Delete' x ('ForkTree 'EmptyTree (Node n a1) 'EmptyTree) 'EQ = 'EmptyTree
  delete' _ (ForkBST EmptyBST (Node _) EmptyBST) _ = EmptyBST
instance Deletable' x ('ForkTree 'EmptyTree (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  type Delete' x ('ForkTree 'EmptyTree (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ =
    'ForkTree rl (Node rn ra) rr
  delete' _ (ForkBST EmptyBST (Node _) r@ForkBST{}) _ = r
instance Deletable' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) 'EmptyTree) 'EQ where
  type Delete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) 'EmptyTree) 'EQ =
    'ForkTree ll (Node ln la) lr
  delete' _ (ForkBST l@ForkBST{} (Node _) EmptyBST) _ = l
instance (l ~ 'ForkTree ll (Node ln la) lr, Show (MaxValue l), MaxKeyDeletable l, Maxable l,
  ProofLtNMaxKeyDelete l n, ProofLTMaxKey l n, LtN (MaxKeyDelete l) (MaxKey l) ~ 'True,
  r ~ 'ForkTree rl (Node rn ra) rr, GtN r (MaxKey l) ~ 'True) =>
  Deletable' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  type Delete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ =
    'ForkTree (MaxKeyDelete ('ForkTree ll (Node ln la) lr))
      (Node (MaxKey ('ForkTree ll (Node ln la) lr)) (MaxValue ('ForkTree ll (Node ln la) lr)))
      ('ForkTree rl (Node rn ra) rr)
  delete' _ (ForkBST l@ForkBST{} (Node _) r@ForkBST{}) _ =
    gcastWith (proofLtNMaxKeyDelete l (Proxy::Proxy n)) $
      gcastWith (proofLTMaxKey l (Proxy::Proxy n)) $
        ForkBST (maxKeyDelete l)
          (Node (maxValue l)::Node (MaxKey ('ForkTree ll (Node ln la) lr))
          (MaxValue ('ForkTree ll (Node ln la) lr))) r
instance Deletable' x ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  type Delete' x ('ForkTree 'EmptyTree (Node n a1) r) 'LT =
    'ForkTree 'EmptyTree (Node n a1) r
  delete' _ t@(ForkBST EmptyBST (Node _) _) _ = t
instance (l ~ 'ForkTree ll (Node ln la) lr, Deletable' x l (CmpNat x ln),
  ProofLtNDelete' x l n (CmpNat x ln)) =>
  Deletable' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) r) 'LT where
  type Delete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) r) 'LT =
    ('ForkTree (Delete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln)) (Node n a1) r)
  delete' px (ForkBST l@ForkBST{} node r) _ =
    gcastWith (proofLtNDelete' px l (Proxy::Proxy n) (Proxy::Proxy (CmpNat x ln))) $
      ForkBST (delete' px l (Proxy::Proxy (CmpNat x ln))) node r
instance Deletable' x ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  type Delete' x ('ForkTree l (Node n a1) 'EmptyTree) 'GT = 'ForkTree l (Node n a1) 'EmptyTree
  delete' _ t@(ForkBST _ (Node _) EmptyBST) _ = t
instance (r ~ 'ForkTree rl (Node rn ra) rr, Deletable' x r (CmpNat x rn),
  ProofGtNDelete' x r n (CmpNat x rn)) =>
  Deletable' x ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'GT where
  type Delete' x ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'GT =
    'ForkTree l (Node n a1) (Delete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn))
  delete' px (ForkBST l node r@ForkBST{}) _ =
    gcastWith (proofGtNDelete' px r (Proxy::Proxy n) (Proxy::Proxy (CmpNat x rn))) $
      ForkBST l node (delete' px r (Proxy::Proxy (CmpNat x rn)))


-- | Prove that deleting a node with key 'x' (lower than 'n')
-- | in a BST 't' which verifies 'LtN t n ~ 'True' preserves the LtN invariant,
-- | given that the comparison between 'x' and the root key of the tree equals 'o'.
-- | The 'o' parameter guides the proof.
class ProofLtNDelete' (x :: Nat) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofLtNDelete' :: (LtN t n ~ 'True) =>
    Proxy x -> BST t -> Proxy n -> Proxy o -> LtN (Delete' x t o) n :~: 'True
instance ProofLtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) 'EmptyTree) n 'EQ where
  proofLtNDelete' _ (ForkBST EmptyBST (Node _) EmptyBST) _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, LtN r n ~ 'True) =>
  ProofLtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ where
  proofLtNDelete' _ (ForkBST EmptyBST (Node _) ForkBST{}) _ _ = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, LtN l n ~ 'True) =>
  ProofLtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) 'EmptyTree) n 'EQ where
  proofLtNDelete' _ (ForkBST ForkBST{} (Node _) EmptyBST) _ _ = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, LtN l n ~ 'True,
  r ~ 'ForkTree rl (Node rn ra) rr, LtN r n ~ 'True,
  ProofLTMaxKey l n, Maxable l, ProofLtNMaxKeyDelete l n, MaxKeyDeletable l) =>
  ProofLtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ where
  proofLtNDelete' _ (ForkBST l@ForkBST{} (Node _) ForkBST{}) _ _ =
    gcastWith (proofLtNMaxKeyDelete l (Proxy::Proxy n)) $
      gcastWith (proofLTMaxKey l (Proxy::Proxy n)) Refl
instance ProofLtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofLtNDelete' _ (ForkBST EmptyBST (Node _) _) _ _ = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, LtN l n ~ 'True, ProofLtNDelete' x l n (CmpNat x ln)) =>
  ProofLtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) r) n 'LT where
  proofLtNDelete' px (ForkBST l@ForkBST{} _ _) _ _ =
    gcastWith (proofLtNDelete' px l (Proxy::Proxy n) (Proxy::Proxy (CmpNat x ln))) Refl
instance ProofLtNDelete' x ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofLtNDelete' _ (ForkBST _ (Node _) EmptyBST) _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, LtN r n ~ 'True,
  ProofLtNDelete' x r n (CmpNat x rn)) =>
  ProofLtNDelete' x ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'GT where
  proofLtNDelete' px (ForkBST _ (Node _) r@ForkBST{}) _ _ =
    gcastWith (proofLtNDelete' px r (Proxy::Proxy n) (Proxy::Proxy (CmpNat x rn))) Refl


-- | Prove that deleting a node with key 'x' (greater than 'n')
-- | in a BST 't' which verifies 'GtN t n ~ 'True' preserves the GtN invariant,
-- | given that the comparison between 'x' and the root key of the tree equals 'o'.
-- | The 'o' parameter guides the proof.
class ProofGtNDelete' (x :: Nat) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofGtNDelete' :: (GtN t n ~ 'True) =>
    Proxy x -> BST t -> Proxy n -> Proxy o -> GtN (Delete' x t o) n :~: 'True
instance ProofGtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) 'EmptyTree) n 'EQ where
  proofGtNDelete' _ (ForkBST EmptyBST (Node _) EmptyBST) _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, GtN r n ~ 'True) =>
  ProofGtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ where
  proofGtNDelete' _ (ForkBST EmptyBST (Node _) ForkBST{}) _ _ = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, GtN l n ~ 'True) =>
  ProofGtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) 'EmptyTree) n 'EQ where
  proofGtNDelete' _ (ForkBST ForkBST{} (Node _) EmptyBST) _ _ = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, GtN l n ~ 'True,
  r ~ 'ForkTree rl (Node rn ra) rr, GtN r n ~ 'True,
  ProofGTMaxKey l n, Maxable l, ProofGtNMaxKeyDelete l n, MaxKeyDeletable l) =>
  ProofGtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) r) n 'EQ where
  proofGtNDelete' _ (ForkBST l@ForkBST{} (Node _) ForkBST{}) _ _ =
    gcastWith (proofGtNMaxKeyDelete l (Proxy::Proxy n)) $
      gcastWith (proofGTMaxKey l (Proxy::Proxy n)) Refl
instance ProofGtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofGtNDelete' _ (ForkBST EmptyBST (Node _) _) _ _ = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, GtN l n ~ 'True,
  ProofGtNDelete' x l n (CmpNat x ln)) =>
  ProofGtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) r) n 'LT where
  proofGtNDelete' px (ForkBST l@ForkBST{} _ _) _ _ =
    gcastWith (proofGtNDelete' px l (Proxy::Proxy n) (Proxy::Proxy (CmpNat x ln))) Refl
instance ProofGtNDelete' x ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofGtNDelete' _ (ForkBST _ (Node _) EmptyBST) _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, GtN r n ~ 'True,
  ProofGtNDelete' x r n (CmpNat x rn)) =>
  ProofGtNDelete' x ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'GT where
  proofGtNDelete' px (ForkBST _ (Node _) r@ForkBST{}) _ _ =
    gcastWith (proofGtNDelete' px r (Proxy::Proxy n) (Proxy::Proxy (CmpNat x rn))) Refl


-- | Prove that in a BST 't' which verifies that 'GtN t n ~ 'True',
-- | the maximum key of 't' is also greater than 'n'.
-- | This proof is needed for the delete operation.
class ProofGTMaxKey (t :: Tree) (n :: Nat) where
  proofGTMaxKey :: (Maxable t, GtN t n ~ 'True) =>
    BST t -> Proxy n -> CmpNat (MaxKey t) n :~: 'GT
instance (CmpNat n1 n ~ 'GT) =>
  ProofGTMaxKey ('ForkTree l (Node n1 a) 'EmptyTree) n where
  proofGTMaxKey (ForkBST _ (Node _) EmptyBST) _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, GtN r n ~ 'True, Maxable r, ProofGTMaxKey r n) =>
  ProofGTMaxKey ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofGTMaxKey (ForkBST _ (Node _) r@ForkBST{}) pn = gcastWith (proofGTMaxKey r pn) Refl

-- | Prove that in a BST 't' which verifies that 'GtN t n ~ 'True',
-- | the tree resulting from the removal of the maximum key of 't' preserves the GtN invariant.
-- | This proof is needed for the delete operation.
class ProofGtNMaxKeyDelete (t :: Tree) (n :: Nat) where
  proofGtNMaxKeyDelete :: (MaxKeyDeletable t, GtN t n ~ 'True) =>
    BST t -> Proxy n -> GtN (MaxKeyDelete t) n :~: 'True
instance (GtN l n ~ 'True) =>
  ProofGtNMaxKeyDelete ('ForkTree l (Node n1 a) 'EmptyTree) n where
  proofGtNMaxKeyDelete (ForkBST _ (Node _) EmptyBST) _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, GtN r n ~ 'True, MaxKeyDeletable r,
  ProofGtNMaxKeyDelete r n) =>
  ProofGtNMaxKeyDelete ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofGtNMaxKeyDelete (ForkBST _ (Node _) r@ForkBST{}) pn =
    gcastWith (proofGtNMaxKeyDelete r pn) Refl

-- | Prove that in a BST 't' which verifies that 'LtN t n ~ 'True',
-- | the maximum key of 't' is also less than 'n'.
-- | This proof is needed for the delete operation.
class ProofLTMaxKey (t :: Tree) (n :: Nat) where
  proofLTMaxKey :: (Maxable t, LtN t n ~ 'True) =>
    BST t -> Proxy n -> CmpNat (MaxKey t) n :~: 'LT
instance (CmpNat n1 n ~ 'LT) =>
  ProofLTMaxKey ('ForkTree l (Node n1 a) 'EmptyTree) n where
  proofLTMaxKey (ForkBST _ (Node _) EmptyBST) _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, LtN r n ~ 'True, Maxable r,
  ProofLTMaxKey r n) =>
  ProofLTMaxKey ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofLTMaxKey (ForkBST _ (Node _) r@ForkBST{}) pn =
    gcastWith (proofLTMaxKey r pn) Refl

-- | Prove that in a BST 't' which verifies that 'LtN t n ~ 'True',
-- | the tree resulting from the removal of the maximum key of 't' preserves the LtN invariant.
-- | This proof is needed for the delete operation.
class ProofLtNMaxKeyDelete (t :: Tree) (n :: Nat) where
  proofLtNMaxKeyDelete :: (MaxKeyDeletable t, LtN t n ~ 'True) =>
    BST t -> Proxy n -> LtN (MaxKeyDelete t) n :~: 'True
instance (LtN l n ~ 'True) =>
  ProofLtNMaxKeyDelete ('ForkTree l (Node n1 a) 'EmptyTree) n where
  proofLtNMaxKeyDelete (ForkBST _ (Node _) EmptyBST) _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, LtN r n ~ 'True, MaxKeyDeletable r,
  ProofLtNMaxKeyDelete r n) =>
  ProofLtNMaxKeyDelete ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofLtNMaxKeyDelete (ForkBST _ (Node _) r@ForkBST{}) pn =
    gcastWith (proofLtNMaxKeyDelete r pn) Refl
