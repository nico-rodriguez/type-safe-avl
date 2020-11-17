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

module Data.Tree.AVL.Intern.Delete (
  Deletable(Delete,delete)
) where

import           Data.Kind                        (Type)
import           Data.Proxy                       (Proxy (Proxy))
import           Data.Tree.AVL.Intern.Balance     (Balanceable (Balance, balance),
                                                   ProofGtNBalance (proofGtNBalance),
                                                   ProofLtNBalance (proofLtNBalance))
import           Data.Tree.AVL.Intern.Constructor (AVL (EmptyAVL, ForkAVL),
                                                   AlmostAVL (AlmostAVL))
import           Data.Tree.BST.Invariants         (GtN, LtN)
import           Data.Tree.ITree                  (Tree (EmptyTree, ForkTree))
import           Data.Tree.Node                   (Node (Node))
import           Data.Type.Equality               ((:~:) (Refl), gcastWith)
import           GHC.TypeLits                     (CmpNat, Nat)
import           Prelude                          (Bool (True),
                                                   Ordering (EQ, GT, LT), Show,
                                                   ($))


-- | Prove that deleting a node with key 'x' (lower than 'n')
-- | in a tree 't' which verifies 'LtN t n ~ 'True' preserves the LtN invariant,
-- | given that the comparison between 'x' and the root key of the tree equals 'o'.
-- | The 'o' parameter guides the proof.
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
instance (l ~ 'ForkTree ll (Node ln la) lr, r ~ 'ForkTree rl (Node rn ra) rr, LtN l n ~ 'True, Show (MaxValue l),
  ProofLTMaxKey l n, Maxable l, ProofLtNMaxKeyDelete l n, MaxKeyDeletable l, LtN r n ~ 'True,
  GtN r (MaxKey l) ~ 'True, LtN (MaxKeyDelete l) (MaxKey l) ~ 'True, ProofLtNBalance ('ForkTree (MaxKeyDelete l) (Node (MaxKey l) (MaxValue l)) r) n) =>
  ProofLtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ where
  proofLtNDelete' _ (ForkAVL l _ r) pn _ =
    gcastWith (proofLtNMaxKeyDelete l pn) $
    gcastWith (proofLTMaxKey l pn) $
    gcastWith (proofLtNBalance (AlmostAVL l' node' r) pn) Refl
      where
        l' = maxKeyDelete l
        node' = Node (maxValue l) :: Node (MaxKey l) (MaxValue l)
instance ProofLtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofLtNDelete' _ _ _ _ = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, o ~ CmpNat x ln, ProofLtNDelete' x l n o, CmpNat x n1 ~ 'LT, Deletable x l,
  LtN l n ~ 'True, ProofLtNBalance ('ForkTree (Delete' x l o) (Node n1 a1) r) n, ProofLtNDelete' x l n1 o) =>
  ProofLtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) r) n 'LT where
  proofLtNDelete' px (ForkAVL l node r) pn _ =
    gcastWith (proofLtNDelete' px l pn (Proxy::Proxy o)) $
    gcastWith (proofLtNDelete' px l (Proxy::Proxy n1) (Proxy::Proxy o)) $
    gcastWith (proofLtNBalance (AlmostAVL l' node r) pn) Refl
      where
        l' = delete px l
instance ProofLtNDelete' x ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofLtNDelete' _ _ _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, o ~ CmpNat x rn, ProofLtNDelete' x r n o, CmpNat x n1 ~ 'GT, Deletable x r,
  LtN r n ~ 'True, ProofLtNBalance ('ForkTree l (Node n1 a1) (Delete' x r o)) n, ProofGtNDelete' x r n1 o) =>
  ProofLtNDelete' x ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'GT where
  proofLtNDelete' px (ForkAVL l node r) pn _ =
    gcastWith (proofLtNDelete' px r pn (Proxy::Proxy o)) $
    gcastWith (proofGtNDelete' px r (Proxy::Proxy n1) (Proxy::Proxy o)) $
    gcastWith (proofLtNBalance (AlmostAVL l node r') pn) Refl
      where
        r' = delete px r


-- | Prove that deleting a node with key 'x' (greater than 'n')
-- | in a tree 't' which verifies 'GtN t n ~ 'True' preserves the GtN invariant,
-- | given that the comparison between 'x' and the root key of the tree equals 'o'.
-- | The 'o' parameter guides the proof.
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
instance (l ~ 'ForkTree ll (Node ln la) lr, r ~ 'ForkTree rl (Node rn ra) rr, GtN l n ~ 'True, Show (MaxValue l),
  ProofGTMaxKey l n, Maxable l, ProofGtNMaxKeyDelete l n, MaxKeyDeletable l, GtN r n ~ 'True,
  GtN r (MaxKey l) ~ 'True, LtN (MaxKeyDelete l) (MaxKey l) ~ 'True, ProofGtNBalance ('ForkTree (MaxKeyDelete l) (Node (MaxKey l) (MaxValue l)) r) n) =>
  ProofGtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ where
  proofGtNDelete' _ (ForkAVL l _ r) pn _ =
    gcastWith (proofGtNMaxKeyDelete l pn) $
    gcastWith (proofGTMaxKey l pn) $
    gcastWith (proofGtNBalance (AlmostAVL l' node' r) pn) Refl
      where
        l' = maxKeyDelete l
        node' = Node (maxValue l) :: Node (MaxKey l) (MaxValue l)
instance ProofGtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofGtNDelete' _ _ _ _ = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, o ~ CmpNat x ln, ProofGtNDelete' x l n o, CmpNat x n1 ~ 'LT, Deletable x l,
  GtN l n ~ 'True, ProofGtNBalance ('ForkTree (Delete' x l o) (Node n1 a1) r) n, ProofLtNDelete' x l n1 o) =>
  ProofGtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) r) n 'LT where
  proofGtNDelete' px (ForkAVL l node r) pn _ =
    gcastWith (proofGtNDelete' px l pn (Proxy::Proxy o)) $
    gcastWith (proofLtNDelete' px l (Proxy::Proxy n1) (Proxy::Proxy o)) $
    gcastWith (proofGtNBalance (AlmostAVL l' node r) pn) Refl
      where
        l' = delete px l
instance ProofGtNDelete' x ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofGtNDelete' _ _ _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, o ~ CmpNat x rn, ProofGtNDelete' x r n o, CmpNat x n1 ~ 'GT, Deletable x r,
  GtN r n ~ 'True, ProofGtNBalance ('ForkTree l (Node n1 a1) (Delete' x r o)) n, ProofGtNDelete' x r n1 o) =>
  ProofGtNDelete' x ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'GT where
  proofGtNDelete' px (ForkAVL l node r) pn _ =
    gcastWith (proofGtNDelete' px r pn (Proxy::Proxy o)) $
    gcastWith (proofGtNDelete' px r (Proxy::Proxy n1) (Proxy::Proxy o)) $
    gcastWith (proofGtNBalance (AlmostAVL l node r') pn) Refl
      where
        r' = delete px r


-- | Prove that in a tree 't' which verifies that 'GtN t n ~ 'True',
-- | the maximum key of 't' is also greater than 'n'.
-- | This proof is needed for the delete operation.
class ProofGTMaxKey (t :: Tree) (n :: Nat) where
  proofGTMaxKey :: (Maxable t, GtN t n ~ 'True) =>
    AVL t -> Proxy n -> CmpNat (MaxKey t) n :~: 'GT
instance (CmpNat n1 n ~ 'GT) => ProofGTMaxKey ('ForkTree l (Node n1 a) 'EmptyTree) n where
  proofGTMaxKey _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, GtN r n ~ 'True, Maxable r, ProofGTMaxKey r n) =>
  ProofGTMaxKey ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofGTMaxKey (ForkAVL _ _ r) pn =
    gcastWith (proofGTMaxKey r pn) Refl

-- | Prove that in a tree 't' which verifies that 'GtN t n ~ 'True',
-- | the tree resulting from the removal of the maximum key of 't' preserves the GtN invariant.
-- | This proof is needed for the delete operation.
class ProofGtNMaxKeyDelete (t :: Tree) (n :: Nat) where
  proofGtNMaxKeyDelete :: (MaxKeyDeletable t, GtN t n ~ 'True) =>
    AVL t -> Proxy n -> GtN (MaxKeyDelete t) n :~: 'True
instance (GtN l n ~ 'True) => ProofGtNMaxKeyDelete ('ForkTree l (Node n1 a) 'EmptyTree) n where
  proofGtNMaxKeyDelete _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, ProofGtNMaxKeyDelete r n, MaxKeyDeletable r, GtN r n ~ 'True,
  ProofGtNBalance ('ForkTree l (Node n1 a) (MaxKeyDelete r)) n, ProofGtNMaxKeyDelete r n1) =>
  ProofGtNMaxKeyDelete ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofGtNMaxKeyDelete (ForkAVL l node r) pn =
    gcastWith (proofGtNMaxKeyDelete r pn) $
    gcastWith (proofGtNMaxKeyDelete r (Proxy::Proxy n1)) $
    gcastWith (proofGtNBalance (AlmostAVL l node r') pn) Refl
      where
        r' = maxKeyDelete r

-- | Prove that in a tree 't' which verifies that 'LtN t n ~ 'True',
-- | the maximum key of 't' is also less than 'n'.
-- | This proof is needed for the delete operation.
class ProofLTMaxKey (t :: Tree) (n :: Nat) where
  proofLTMaxKey :: (Maxable t, LtN t n ~ 'True) =>
    AVL t -> Proxy n -> CmpNat (MaxKey t) n :~: 'LT
instance (CmpNat n1 n ~ 'LT) => ProofLTMaxKey ('ForkTree l (Node n1 a) 'EmptyTree) n where
  proofLTMaxKey _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, Maxable r, ProofLTMaxKey r n, LtN r n ~ 'True) =>
  ProofLTMaxKey ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofLTMaxKey (ForkAVL _ _ r) pn =
    gcastWith (proofLTMaxKey r pn) Refl

-- | Prove that in a tree 't' which verifies that 'LtN t n ~ 'True',
-- | the tree resulting from the removal of the maximum key of 't' preserves the LtN invariant.
-- | This proof is needed for the delete operation.
class ProofLtNMaxKeyDelete (t :: Tree) (n :: Nat) where
  proofLtNMaxKeyDelete :: (MaxKeyDeletable t, LtN t n ~ 'True) =>
    AVL t -> Proxy n -> LtN (MaxKeyDelete t) n :~: 'True
instance (LtN l n ~ 'True) => ProofLtNMaxKeyDelete ('ForkTree l (Node n1 a) 'EmptyTree) n where
  proofLtNMaxKeyDelete _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, ProofLtNMaxKeyDelete r n, MaxKeyDeletable r, LtN r n ~ 'True, ProofGtNMaxKeyDelete r n1,
  ProofLtNBalance ('ForkTree l (Node n1 a) (MaxKeyDelete r)) n, LtN r n1 ~ 'True) =>
  ProofLtNMaxKeyDelete ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofLtNMaxKeyDelete (ForkAVL l node r) pn =
    gcastWith (proofLtNMaxKeyDelete r pn) $
    gcastWith (proofGtNMaxKeyDelete r (Proxy::Proxy n1)) $
    gcastWith (proofLtNBalance (AlmostAVL l node r') pn) Refl
      where
        r' = maxKeyDelete r


-- | This class provides the functionality to delete the node with maximum key value
-- | in an AVL 't'.
-- | The deletion is defined at the value level and the type level.
class MaxKeyDeletable (t :: Tree) where
  type MaxKeyDelete (t :: Tree) :: Tree
  maxKeyDelete :: (t ~ 'ForkTree l (Node n a1) r) =>
    AVL t -> AVL (MaxKeyDelete t)
instance MaxKeyDeletable ('ForkTree l (Node n a1) 'EmptyTree) where
  type MaxKeyDelete ('ForkTree l (Node n a1) 'EmptyTree) = l
  maxKeyDelete (ForkAVL l (Node _) EmptyAVL) = l
instance (MaxKeyDeletable ('ForkTree rl (Node rn ra) rr), ProofGtNMaxKeyDelete ('ForkTree rl (Node rn ra) rr) n,
  Balanceable ('ForkTree l (Node n a1) (MaxKeyDelete ('ForkTree rl (Node rn ra) rr)))) =>
  MaxKeyDeletable ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) where
  type MaxKeyDelete ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) =
    Balance ('ForkTree l (Node n a1) (MaxKeyDelete ('ForkTree rl (Node rn ra) rr)))
  maxKeyDelete (ForkAVL l node r@ForkAVL{}) =
    gcastWith (proofGtNMaxKeyDelete r (Proxy::Proxy n)) $
    balance $ AlmostAVL l node (maxKeyDelete r)

-- | This class provides the functionality to get the key, type and value of the node with maximum key value
-- | in an AVL 't'.
-- | The lookup is defined at the value level and the type level.
-- | Since the keys are only kept at the type level, there's no value level getter of the maximum key.
class Maxable (t :: Tree) where
  type MaxKey (t :: Tree) :: Nat
  type MaxValue (t :: Tree) :: Type
  maxValue :: (t ~ 'ForkTree l (Node n a1) r, a ~ MaxValue t) =>
    AVL t -> a
instance Maxable ('ForkTree l (Node n a1) 'EmptyTree) where
  type MaxKey ('ForkTree l (Node n a1) 'EmptyTree) = n
  type MaxValue ('ForkTree l (Node n a1) 'EmptyTree) = a1
  maxValue (ForkAVL _ (Node a1) EmptyAVL) = a1
instance Maxable ('ForkTree rl (Node rn ra) rr) =>
  Maxable ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) where
  type MaxKey ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) = MaxKey ('ForkTree rl (Node rn ra) rr)
  type MaxValue ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) = MaxValue ('ForkTree rl (Node rn ra) rr)
  maxValue (ForkAVL _ (Node _) r@ForkAVL{}) = maxValue r


-- | This class provides the functionality to delete the node with key 'x'
-- | in an AVL 't'.
-- | The deletion is defined at the value level and the type level,
-- | and both return AVL trees.
class Deletable (x :: Nat) (t :: Tree) where
  type Delete (x :: Nat) (t :: Tree) :: Tree
  delete :: Proxy x -> AVL t -> AVL (Delete x t)
instance Deletable x 'EmptyTree where
  type Delete x 'EmptyTree = 'EmptyTree
  delete _ EmptyAVL = EmptyAVL
instance (Deletable' x ('ForkTree l (Node n a1) r) (CmpNat x n)) =>
  Deletable x ('ForkTree l (Node n a1) r) where
  type Delete x ('ForkTree l (Node n a1) r) = Delete' x ('ForkTree l (Node n a1) r) (CmpNat x n)
  delete px t = delete' px t (Proxy::Proxy (CmpNat x n))

-- | This class provides the functionality to delete a node with key 'x'
-- | in a non empty AVL 't'.
-- | It's only used by the 'Deletable' class and it has one extra parameter 'o',
-- | which is the type level comparison of 'x' with the key value of the root node.
-- | The 'o' parameter guides the insertion.
class Deletable' (x :: Nat) (t :: Tree) (o :: Ordering) where
  type Delete' (x :: Nat) (t :: Tree) (o :: Ordering) :: Tree
  delete' :: Proxy x -> AVL t -> Proxy o -> AVL (Delete' x t o)
instance Deletable' x ('ForkTree 'EmptyTree (Node n a1) 'EmptyTree) 'EQ where
  type Delete' x ('ForkTree 'EmptyTree (Node n a1) 'EmptyTree) 'EQ = 'EmptyTree
  delete' _ (ForkAVL EmptyAVL (Node _) EmptyAVL) _ = EmptyAVL
instance Deletable' x ('ForkTree 'EmptyTree (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  type Delete' x ('ForkTree 'EmptyTree (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ = ('ForkTree rl (Node rn ra) rr)
  delete' _ (ForkAVL EmptyAVL (Node _) r@ForkAVL{}) _ = r
instance Deletable' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) 'EmptyTree) 'EQ where
  type Delete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) 'EmptyTree) 'EQ = ('ForkTree ll (Node ln la) lr)
  delete' _ (ForkAVL l@ForkAVL{} (Node _) EmptyAVL) _ = l
instance (l ~ 'ForkTree ll (Node ln la) lr, Show (MaxValue l), MaxKeyDeletable ('ForkTree ll (Node ln la) lr),
  r ~ 'ForkTree rl (Node rn ra) rr, Maxable ('ForkTree ll (Node ln la) lr), LtN (MaxKeyDelete l) (MaxKey l) ~ 'True,
  GtN r (MaxKey l) ~ 'True,
  Balanceable ('ForkTree (MaxKeyDelete ('ForkTree ll (Node ln la) lr)) (Node (MaxKey ('ForkTree ll (Node ln la) lr)) (MaxValue ('ForkTree ll (Node ln la) lr))) r)) =>
  Deletable' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  type Delete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ =
    Balance ('ForkTree (MaxKeyDelete ('ForkTree ll (Node ln la) lr)) (Node (MaxKey ('ForkTree ll (Node ln la) lr)) (MaxValue ('ForkTree ll (Node ln la) lr))) ('ForkTree rl (Node rn ra) rr))
  delete' _ (ForkAVL l@ForkAVL{} (Node _) r@ForkAVL{}) _ =
    balance $
    AlmostAVL (maxKeyDelete l) (Node (maxValue l)::Node (MaxKey ('ForkTree ll (Node ln la) lr)) (MaxValue ('ForkTree ll (Node ln la) lr))) r
instance Deletable' x ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  type Delete' x ('ForkTree 'EmptyTree (Node n a1) r) 'LT = ('ForkTree 'EmptyTree (Node n a1) r)
  delete' _ t@(ForkAVL EmptyAVL (Node _) _) _ = t
instance (Deletable' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln),
  Balanceable ('ForkTree (Delete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln)) (Node n a1) r),
  ProofLtNDelete' x ('ForkTree ll (Node ln la) lr) n (CmpNat x ln), CmpNat x n ~ 'LT) =>
  Deletable' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) r) 'LT where
  type Delete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) r) 'LT =
    Balance ('ForkTree (Delete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln)) (Node n a1) r)
  delete' px (ForkAVL l@ForkAVL{} node r) _ =
    gcastWith (proofLtNDelete' px l (Proxy::Proxy n) (Proxy::Proxy (CmpNat x ln))) $
    balance $
    AlmostAVL (delete' px l (Proxy::Proxy (CmpNat x ln))) node r
instance Deletable' x ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  type Delete' x ('ForkTree l (Node n a1) 'EmptyTree) 'GT = ('ForkTree l (Node n a1) 'EmptyTree)
  delete' _ t@(ForkAVL _ (Node _) EmptyAVL) _ = t
instance (Deletable' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn),
  Balanceable ('ForkTree l (Node n a1) (Delete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn))),
  ProofGtNDelete' x ('ForkTree rl (Node rn ra) rr) n (CmpNat x rn), CmpNat x n ~ 'GT) =>
  Deletable' x ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'GT where
  type Delete' x ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'GT =
    Balance ('ForkTree l (Node n a1) (Delete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn)))
  delete' px (ForkAVL l node r@ForkAVL{}) _ =
      gcastWith (proofGtNDelete' px r (Proxy::Proxy n) (Proxy::Proxy (CmpNat x rn))) $
      balance $
      AlmostAVL l node (delete' px r (Proxy::Proxy (CmpNat x rn)))
