{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# LANGUAGE Safe                  #-}

module Data.Tree.AVL.Extern.DeleteProofs (
  IsAVLT(..),
  ProofIsAVLDelete(proofIsAVLDelete),
  ProofIsBSTDelete(proofIsBSTDelete)
) where

import           Data.Proxy                         (Proxy (Proxy))
import           Data.Tree.AVL.Extern.BalanceProofs (ProofGtNBalance (proofGtNBalance),
                                                     ProofIsAVLBalance (proofIsAVLBalance),
                                                     ProofIsBSTBalance (proofIsBSTBalance),
                                                     ProofLtNBalance (proofLtNBalance))
import           Data.Tree.AVL.Extern.Constructors  (IsAVLT(EmptyIsAVLT,ForkIsAVLT), IsAlmostAVLT(ForkIsAlmostAVLT))
import           Data.Tree.AVL.Extern.Delete        (Deletable (Delete),
                                                     Deletable' (Delete'),
                                                     MaxKeyDeletable (MaxKeyDelete))
import           Data.Tree.BST.Extern.Delete        (MaxKey, MaxValue)
import           Data.Tree.BST.Invariants           (GtN, LtN)
import           Data.Tree.BST.Extern.Constructors  (IsBSTT(EmptyIsBSTT,ForkIsBSTT))
import           Data.Tree.ITree                    (Tree (EmptyTree, ForkTree))
import           Data.Tree.Node                     (Node)
import           Data.Type.Equality                 ((:~:) (Refl), gcastWith)
import           GHC.TypeNats                       (CmpNat, Nat)
import           Prelude                            (Bool (True), undefined,
                                                     Ordering (EQ, GT, LT), ($))


-- | Prove that deleting a node with key 'x'
-- | in an AVL tree preserves the AVL condition.
class ProofIsBSTDelete (x :: Nat) (t :: Tree) where
  proofIsBSTDelete :: Proxy x -> IsBSTT t -> IsBSTT (Delete x t)
instance ProofIsBSTDelete x 'EmptyTree where
  proofIsBSTDelete _ _ = EmptyIsBSTT
instance (o ~ CmpNat x n,
  ProofIsBSTDelete' x ('ForkTree l (Node n a1) r) o) =>
  ProofIsBSTDelete x ('ForkTree l (Node n a1) r) where
  proofIsBSTDelete px tIsBST = proofIsBSTDelete' px tIsBST (Proxy::Proxy o)

-- | Prove that inserting a node with key 'x' and element value 'a'
-- | in an AVL tree preserves the AVL condition, given that the comparison between
-- | 'x' and the root key of the tree equals 'o'.
-- | The AVL invariant was already check when proofIsBSTInsert was called before.
-- | The 'o' parameter guides the proof.
class ProofIsBSTDelete' (x :: Nat) (t :: Tree) (o :: Ordering) where
  proofIsBSTDelete' :: Proxy x -> IsBSTT t -> Proxy o -> IsBSTT (Delete' x t o)
instance ProofIsBSTDelete' x ('ForkTree 'EmptyTree (Node n a1) 'EmptyTree) 'EQ where
  proofIsBSTDelete' _ _ _ = EmptyIsBSTT
instance ProofIsBSTDelete' x ('ForkTree 'EmptyTree (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  proofIsBSTDelete' _ (ForkIsBSTT _ _ rIsBST) _ = rIsBST
instance ProofIsBSTDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) 'EmptyTree) 'EQ where
  proofIsBSTDelete' _ (ForkIsBSTT lIsBST _ _) _ = lIsBST
instance (l ~ 'ForkTree ll (Node ln la) lr, r ~ 'ForkTree rl (Node rn ra) rr,
  LtN (MaxKeyDelete l) (MaxKey l) ~ 'True, GtN r (MaxKey l) ~ 'True,
  ProofMaxKeyDeleteIsBST l, ProofIsBSTBalance ('ForkTree (MaxKeyDelete l) (Node (MaxKey l) (MaxValue l)) r)) =>
  ProofIsBSTDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  proofIsBSTDelete' _ (ForkIsBSTT lIsBST _ rIsBST) _ =
    proofIsBSTBalance $ ForkIsBSTT (proofMaxKeyDeleteIsBST lIsBST) node rIsBST
      where
        node = undefined::Node (MaxKey l) (MaxValue l)
instance ProofIsBSTDelete' x ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  proofIsBSTDelete' _ tIsBST _ = tIsBST
instance (l ~ 'ForkTree ll (Node ln la) lr, o ~ CmpNat x ln,
  CmpNat x n ~ 'LT,
  ProofIsBSTDelete' x l o, ProofLtNDelete' x l n o, ProofIsBSTBalance ('ForkTree (Delete' x l o) (Node n a1) r)) =>
  ProofIsBSTDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) r) 'LT where
  proofIsBSTDelete' px (ForkIsBSTT lIsBST node rIsBST) _ =
    gcastWith (proofLtNDelete' px lIsBST (Proxy::Proxy n) (Proxy::Proxy o)) $
    proofIsBSTBalance $ ForkIsBSTT (proofIsBSTDelete px lIsBST) node rIsBST
instance ProofIsBSTDelete' x ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  proofIsBSTDelete' _ tIsBST _ = tIsBST
instance (r ~ 'ForkTree rl (Node rn ra) rr, o ~ CmpNat x rn,
  CmpNat x n ~ 'GT,
  ProofIsBSTDelete' x r o, ProofGtNDelete' x r n o, ProofIsBSTBalance ('ForkTree l (Node n a1) (Delete' x r o))) =>
  ProofIsBSTDelete' x ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'GT where
  proofIsBSTDelete' px (ForkIsBSTT lIsBST node rIsBST) _ =
    gcastWith (proofGtNDelete' px rIsBST (Proxy::Proxy n) (Proxy::Proxy o)) $
    proofIsBSTBalance $ ForkIsBSTT lIsBST node (proofIsBSTDelete px rIsBST)


-- | Prove that deleting a node with key 'x' (lower than 'n')
-- | in a tree 't' which verifies 'LtN t n ~ 'True' preserves the LtN invariant,
-- | given that the comparison between 'x' and the root key of the tree equals 'o'.
-- | The 'o' parameter guides the proof.
class ProofLtNDelete' (x :: Nat) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofLtNDelete' :: (CmpNat x n ~ 'LT, LtN t n ~ 'True) =>
    Proxy x -> IsBSTT t -> Proxy n -> Proxy o -> LtN (Delete' x t o) n :~: 'True
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
  ProofLTMaxKey l n, ProofLtNMaxKeyDelete l n, ProofMaxKeyDeleteIsBST l,
  ProofLtNBalance ('ForkTree (MaxKeyDelete l) (Node (MaxKey l) (MaxValue l)) r) n) =>
  ProofLtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ where
  proofLtNDelete' _ (ForkIsBSTT lIsBST _ rIsBST) pn _ =
    gcastWith (proofLtNMaxKeyDelete lIsBST pn) $
    gcastWith (proofLTMaxKey lIsBST pn) $
    gcastWith (proofLtNBalance (ForkIsBSTT lIsBST' node' rIsBST) pn) Refl
      where
        lIsBST' = proofMaxKeyDeleteIsBST lIsBST
        node'   = undefined::Node (MaxKey l) (MaxValue l)
instance ProofLtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofLtNDelete' _ _ _ _ = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, o ~ CmpNat x ln,
  LtN l n ~ 'True, LtN r n ~ 'True, CmpNat x n1 ~ 'LT,
  ProofLtNDelete' x l n o, ProofIsBSTDelete x l, ProofLtNDelete' x l n1 o,
  ProofLtNBalance ('ForkTree (Delete' x l o) (Node n1 a1) r) n) =>
  ProofLtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) r) n 'LT where
  proofLtNDelete' px (ForkIsBSTT lIsBST node rIsBST) pn _ =
    gcastWith (proofLtNDelete' px lIsBST pn po) $
    gcastWith (proofLtNDelete' px lIsBST (Proxy::Proxy n1) po) $
    gcastWith (proofLtNBalance (ForkIsBSTT lIsBST' node rIsBST) pn) Refl
      where
        po      = Proxy::Proxy o
        lIsBST' = proofIsBSTDelete px lIsBST
instance ProofLtNDelete' x ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofLtNDelete' _ _ _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, o ~ CmpNat x rn,
  CmpNat x n1 ~ 'GT, LtN r n ~ 'True,
  ProofLtNDelete' x r n o, ProofGtNDelete' x r n1 o, ProofIsBSTDelete x r,
  ProofLtNBalance ('ForkTree l (Node n1 a1) (Delete' x r o)) n) =>
  ProofLtNDelete' x ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'GT where
  proofLtNDelete' px (ForkIsBSTT lIsBST node rIsBST) pn _ =
    gcastWith (proofLtNDelete' px rIsBST pn po) $
    gcastWith (proofGtNDelete' px rIsBST (Proxy::Proxy n1) po) $
    gcastWith (proofLtNBalance (ForkIsBSTT lIsBST node rIsBST') pn) Refl
      where
        po      = Proxy::Proxy o
        rIsBST' = proofIsBSTDelete px rIsBST


-- | Prove that deleting a node with key 'x' (greater than 'n')
-- | in a tree 't' which verifies 'GtN t n ~ 'True' preserves the GtN invariant,
-- | given that the comparison between 'x' and the root key of the tree equals 'o'.
-- | The 'o' parameter guides the proof.
class ProofGtNDelete' (x :: Nat) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofGtNDelete' :: (CmpNat x n ~ 'GT, GtN t n ~ 'True) =>
    Proxy x -> IsBSTT t -> Proxy n -> Proxy o -> GtN (Delete' x t o) n :~: 'True
instance ProofGtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) 'EmptyTree) n 'EQ where
  proofGtNDelete' _ _ _ _ = Refl
instance (GtN ('ForkTree rl (Node rn ra) rr) n ~ 'True) =>
  ProofGtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ where
  proofGtNDelete' _ _ _ _ = Refl
instance (GtN ('ForkTree ll (Node ln la) lr) n ~ 'True) =>
  ProofGtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) 'EmptyTree) n 'EQ where
  proofGtNDelete' _ _ _ _ = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, r ~ 'ForkTree rl (Node rn ra) rr,
  MaxKeyDeletable l,
  GtN l n ~ 'True, GtN r n ~ 'True, GtN r (MaxKey l) ~ 'True, LtN (MaxKeyDelete l) (MaxKey l) ~ 'True,
  ProofGTMaxKey l n, ProofGtNMaxKeyDelete l n, ProofMaxKeyDeleteIsBST l,
  ProofGtNBalance ('ForkTree (MaxKeyDelete l) (Node (MaxKey l) (MaxValue l)) r) n) =>
  ProofGtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ where
  proofGtNDelete' _ (ForkIsBSTT lIsBST _ rIsBST) pn _ =
    gcastWith (proofGtNMaxKeyDelete lIsBST pn) $
    gcastWith (proofGTMaxKey lIsBST pn) $
    gcastWith (proofGtNBalance (ForkIsBSTT lIsBST' node' rIsBST) pn) Refl
      where
        lIsBST' = proofMaxKeyDeleteIsBST lIsBST
        node'   = undefined::Node (MaxKey l) (MaxValue l)
instance ProofGtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofGtNDelete' _ _ _ _ = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, o ~ CmpNat x ln,
  CmpNat x n1 ~ 'LT, GtN l n ~ 'True, GtN r n ~ 'True,
  ProofLtNDelete' x l n1 o, ProofGtNDelete' x l n o, ProofIsBSTDelete x l,
  ProofGtNBalance ('ForkTree (Delete' x l o) (Node n1 a1) r) n) =>
  ProofGtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) r) n 'LT where
  proofGtNDelete' px (ForkIsBSTT lIsBST node rIsBST) pn _ =
    gcastWith (proofGtNDelete' px lIsBST pn po) $
    gcastWith (proofLtNDelete' px lIsBST (Proxy::Proxy n1) po) $
    gcastWith (proofGtNBalance (ForkIsBSTT lIsBST' node rIsBST) pn) Refl
      where
        po      = Proxy::Proxy o
        lIsBST' = proofIsBSTDelete px lIsBST
instance ProofGtNDelete' x ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofGtNDelete' _ _ _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, o ~ CmpNat x rn,
  CmpNat x n1 ~ 'GT, GtN r n ~ 'True,
  ProofGtNDelete' x r n o, ProofGtNDelete' x r n1 o, ProofIsBSTDelete x r,
  ProofGtNBalance ('ForkTree l (Node n1 a1) (Delete' x r o)) n) =>
  ProofGtNDelete' x ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'GT where
  proofGtNDelete' px (ForkIsBSTT lIsBST node rIsBST) pn _ =
    gcastWith (proofGtNDelete' px rIsBST pn po) $
    gcastWith (proofGtNDelete' px rIsBST (Proxy::Proxy n1) po) $
    gcastWith (proofGtNBalance (ForkIsBSTT lIsBST node rIsBST') pn) Refl
      where
        po      = Proxy::Proxy o
        rIsBST' = proofIsBSTDelete px rIsBST


-- | Prove that deleting the node with maximum key value
-- | in a BST 't' preserves the BST invariant.
-- | This proof is needed for the delete operation.
class ProofMaxKeyDeleteIsBST (t :: Tree) where
  proofMaxKeyDeleteIsBST :: IsBSTT t -> IsBSTT (MaxKeyDelete t)
instance ProofMaxKeyDeleteIsBST 'EmptyTree where
  proofMaxKeyDeleteIsBST _ = EmptyIsBSTT
instance ProofMaxKeyDeleteIsBST ('ForkTree 'EmptyTree (Node n a) 'EmptyTree) where
  proofMaxKeyDeleteIsBST _ = EmptyIsBSTT
instance ProofMaxKeyDeleteIsBST ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) 'EmptyTree) where
  proofMaxKeyDeleteIsBST (ForkIsBSTT lIsBST _ _) = lIsBST
instance (l ~ 'ForkTree ll (Node ln la) lr, r ~ 'ForkTree rl (Node rn ra) rr,
  MaxKeyDeletable r,
  ProofGtNMaxKeyDelete r n, ProofMaxKeyDeleteIsBST r,
  ProofIsBSTBalance ('ForkTree l (Node n a) (MaxKeyDelete r))) =>
  ProofMaxKeyDeleteIsBST ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) ('ForkTree rl (Node rn ra) rr)) where
  proofMaxKeyDeleteIsBST (ForkIsBSTT lIsBST node rIsBST) =
    gcastWith (proofGtNMaxKeyDelete rIsBST (Proxy::Proxy n)) $
    proofIsBSTBalance (ForkIsBSTT lIsBST node rIsBST')
      where
        rIsBST' = proofMaxKeyDeleteIsBST rIsBST


-- | Prove that in a tree 't' which verifies that 'GtN t n ~ 'True',
-- | the maximum key of 't' is also greater than 'n'.
-- | This proof is needed for the delete operation.
class ProofGTMaxKey (t :: Tree) (n :: Nat) where
  proofGTMaxKey :: (GtN t n ~ 'True) =>
    IsBSTT t -> Proxy n -> CmpNat (MaxKey t) n :~: 'GT
instance (CmpNat n1 n ~ 'GT) =>
  ProofGTMaxKey ('ForkTree l (Node n1 a) 'EmptyTree) n where
  proofGTMaxKey _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr,
  GtN r n ~ 'True,
  ProofGTMaxKey r n) =>
  ProofGTMaxKey ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofGTMaxKey (ForkIsBSTT _ _ rIsBST) pn =
    gcastWith (proofGTMaxKey rIsBST pn) Refl

-- | Prove that in a tree 't' which verifies that 'GtN t n ~ 'True',
-- | the tree resulting from the removal of the maximum key of 't' preserves the GtN invariant.
-- | This proof is needed for the delete operation.
class ProofGtNMaxKeyDelete (t :: Tree) (n :: Nat) where
  proofGtNMaxKeyDelete :: (MaxKeyDeletable t, GtN t n ~ 'True) =>
    IsBSTT t -> Proxy n -> GtN (MaxKeyDelete t) n :~: 'True
instance (GtN l n ~ 'True) =>
  ProofGtNMaxKeyDelete ('ForkTree l (Node n1 a) 'EmptyTree) n where
  proofGtNMaxKeyDelete _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr,
  GtN l n ~ 'True, CmpNat n1 n ~ 'GT,
  MaxKeyDeletable r,
  ProofGtNMaxKeyDelete r n, ProofGtNMaxKeyDelete r n1, ProofMaxKeyDeleteIsBST r,
  ProofGtNBalance ('ForkTree l (Node n1 a) (MaxKeyDelete r)) n) =>
  ProofGtNMaxKeyDelete ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofGtNMaxKeyDelete (ForkIsBSTT lIsBST node rIsBST) pn =
    gcastWith (proofGtNMaxKeyDelete rIsBST pn) $
    gcastWith (proofGtNMaxKeyDelete rIsBST (Proxy::Proxy n1)) $
    gcastWith (proofGtNBalance (ForkIsBSTT lIsBST node rIsBST') pn) Refl
      where
        rIsBST' = proofMaxKeyDeleteIsBST rIsBST

-- | Prove that in a tree 't' which verifies that 'LtN t n ~ 'True',
-- | the maximum key of 't' is also less than 'n'.
-- | This proof is needed for the delete operation.
class ProofLTMaxKey (t :: Tree) (n :: Nat) where
  proofLTMaxKey :: (LtN t n ~ 'True) =>
    IsBSTT t -> Proxy n -> CmpNat (MaxKey t) n :~: 'LT
instance (CmpNat n1 n ~ 'LT) =>
  ProofLTMaxKey ('ForkTree l (Node n1 a) 'EmptyTree) n where
  proofLTMaxKey _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr,
  LtN r n ~ 'True,
  ProofLTMaxKey r n) =>
  ProofLTMaxKey ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofLTMaxKey (ForkIsBSTT _ _ rIsBST) pn =
    gcastWith (proofLTMaxKey rIsBST pn) Refl

-- | Prove that in a tree 't' which verifies that 'LtN t n ~ 'True',
-- | the tree resulting from the removal of the maximum key of 't' preserves the LtN invariant.
-- | This proof is needed for the delete operation.
class ProofLtNMaxKeyDelete (t :: Tree) (n :: Nat) where
  proofLtNMaxKeyDelete :: (LtN t n ~ 'True) =>
    IsBSTT t -> Proxy n -> LtN (MaxKeyDelete t) n :~: 'True
instance (LtN l n ~ 'True) =>
  ProofLtNMaxKeyDelete ('ForkTree l (Node n1 a) 'EmptyTree) n where
  proofLtNMaxKeyDelete _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr,
  LtN r n ~ 'True,
  MaxKeyDeletable r,
  ProofGtNMaxKeyDelete r n1, ProofMaxKeyDeleteIsBST r, ProofLtNMaxKeyDelete r n,
  ProofLtNBalance ('ForkTree l (Node n1 a) (MaxKeyDelete r)) n) =>
  ProofLtNMaxKeyDelete ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofLtNMaxKeyDelete (ForkIsBSTT lIsBST node rIsBST) pn =
    gcastWith (proofLtNMaxKeyDelete rIsBST pn) $
    gcastWith (proofGtNMaxKeyDelete rIsBST (Proxy::Proxy n1)) $
    gcastWith (proofLtNBalance (ForkIsBSTT lIsBST node rIsBST') pn) Refl
      where
        rIsBST' = proofMaxKeyDeleteIsBST rIsBST


-- | Prove that deleting the node with maximum key value
-- | in an AVL 't' preserves the AVL invariant.
-- | This proof is needed for the delete operation.
class ProofMaxKeyDeleteIsAVL (t :: Tree) where
  proofMaxKeyDeleteIsAVL :: IsAVLT t -> IsAVLT (MaxKeyDelete t)
instance ProofMaxKeyDeleteIsAVL 'EmptyTree where
  proofMaxKeyDeleteIsAVL _ = EmptyIsAVLT
instance ProofMaxKeyDeleteIsAVL ('ForkTree 'EmptyTree (Node n a) 'EmptyTree) where
  proofMaxKeyDeleteIsAVL _ = EmptyIsAVLT
instance ProofMaxKeyDeleteIsAVL ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) 'EmptyTree) where
  proofMaxKeyDeleteIsAVL (ForkIsAVLT lIsAVL _ _) = lIsAVL
instance (l ~ 'ForkTree ll (Node ln la) lr, r ~ 'ForkTree rl (Node rn ra) rr,
  ProofMaxKeyDeleteIsAVL r, ProofIsAVLBalance ('ForkTree l (Node n a) (MaxKeyDelete r))) =>
  ProofMaxKeyDeleteIsAVL ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) ('ForkTree rl (Node rn ra) rr)) where
  proofMaxKeyDeleteIsAVL (ForkIsAVLT lIsAVL node rIsAVL) =
    proofIsAVLBalance $ ForkIsAlmostAVLT lIsAVL node (proofMaxKeyDeleteIsAVL rIsAVL)


-- | Prove that deleting a node with key 'x'
-- | in an AVL tree preserves the AVL condition.
class ProofIsAVLDelete (x :: Nat) (t :: Tree) where
  proofIsAVLDelete :: Proxy x -> IsAVLT t -> IsAVLT (Delete x t)
instance ProofIsAVLDelete x 'EmptyTree where
  proofIsAVLDelete _ _ = EmptyIsAVLT
instance (o ~ CmpNat x n,
  ProofIsAVLDelete' x ('ForkTree l (Node n a1) r) o) =>
  ProofIsAVLDelete x ('ForkTree l (Node n a1) r) where
  proofIsAVLDelete px tIsAVL = proofIsAVLDelete' px tIsAVL (Proxy::Proxy o)

-- | Prove that deleting a node with key 'x'
-- | in an AVL tree preserves the AVL condition, given that the comparison between
-- | 'x' and the root key of the tree equals 'o'.
-- | The AVL invariant was already check when proofIsBSTDelete was called before.
-- | The 'o' parameter guides the proof.
class ProofIsAVLDelete' (x :: Nat) (t :: Tree) (o :: Ordering) where
  proofIsAVLDelete' :: Proxy x -> IsAVLT t -> Proxy o -> IsAVLT (Delete' x t o)
instance ProofIsAVLDelete' x ('ForkTree 'EmptyTree (Node n a1) 'EmptyTree) 'EQ where
  proofIsAVLDelete' _ _ _ = EmptyIsAVLT
instance ProofIsAVLDelete' x ('ForkTree 'EmptyTree (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  proofIsAVLDelete' _ (ForkIsAVLT _ _ rIsAVL) _ = rIsAVL
instance ProofIsAVLDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) 'EmptyTree) 'EQ where
  proofIsAVLDelete' _ (ForkIsAVLT lIsAVL _ _) _ = lIsAVL
instance (l ~ 'ForkTree ll (Node ln la) lr, r ~ 'ForkTree rl (Node rn ra) rr,
  ProofMaxKeyDeleteIsAVL l, ProofIsAVLBalance ('ForkTree (MaxKeyDelete l) (Node (MaxKey l) (MaxValue l)) r)) =>
  ProofIsAVLDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  proofIsAVLDelete' _ (ForkIsAVLT lIsAVL _ rIsAVL) _ =
    proofIsAVLBalance (ForkIsAlmostAVLT lIsAVL' node rIsAVL)
      where
        lIsAVL' = proofMaxKeyDeleteIsAVL lIsAVL
        node    = undefined::Node (MaxKey l) (MaxValue l)
instance ProofIsAVLDelete' x ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  proofIsAVLDelete' _ tIsAVL _ = tIsAVL
instance (l ~ 'ForkTree ll (Node ln la) lr,
  ProofIsAVLDelete x l, ProofIsAVLBalance ('ForkTree (Delete' x l (CmpNat x ln)) (Node n a1) r)) =>
  ProofIsAVLDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) r) 'LT where
  proofIsAVLDelete' px (ForkIsAVLT lIsAVL node rIsAVL) _ =
    proofIsAVLBalance (ForkIsAlmostAVLT lIsAVL' node rIsAVL)
      where
        lIsAVL' = proofIsAVLDelete px lIsAVL
instance ProofIsAVLDelete' x ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  proofIsAVLDelete' _ tIsAVL _ = tIsAVL
instance (r ~ 'ForkTree rl (Node rn ra) rr,
  ProofIsAVLDelete x r, ProofIsAVLBalance ('ForkTree l (Node n a1) (Delete' x r (CmpNat x rn)))) =>
  ProofIsAVLDelete' x ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'GT where
  proofIsAVLDelete' px (ForkIsAVLT lIsAVL node rIsAVL) _ =
    proofIsAVLBalance (ForkIsAlmostAVLT lIsAVL node rIsAVL')
      where
        rIsAVL' = proofIsAVLDelete px rIsAVL
