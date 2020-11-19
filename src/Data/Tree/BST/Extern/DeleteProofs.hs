{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# LANGUAGE Safe               #-}

module Data.Tree.BST.Extern.DeleteProofs (
  ProofIsBSTDelete(proofIsBSTDelete)
) where

import           Data.Proxy                       (Proxy (Proxy))
import           Data.Tree.BST.Extern.Constructor (BST (BST))
import           Data.Tree.BST.Extern.Delete      (Deletable (Delete),
                                                   Deletable' (Delete'),
                                                   MaxKeyDeletable (MaxKeyDelete, MaxKeyDelete),
                                                   Maxable (MaxKey, MaxValue))
import           Data.Tree.BST.Invariants         (GtN, IsBSTT(EmptyIsBSTT,ForkIsBSTT), LtN)
import           Data.Tree.ITree                  (Tree (EmptyTree, ForkTree))
import           Data.Tree.Node                   (Node)
import           Data.Type.Equality               ((:~:) (Refl), gcastWith)
import           GHC.TypeNats                     (CmpNat, Nat)
import           Prelude                          (Bool (True), undefined,
                                                   Ordering (EQ, GT, LT), ($))


-- | Prove that deleting a node with key 'x'
-- | in a BST tree preserves BST condition.
class ProofIsBSTDelete (x :: Nat) (t :: Tree) where
  proofIsBSTDelete :: Proxy x -> BST t -> IsBSTT (Delete x t)
instance ProofIsBSTDelete x 'EmptyTree where
  proofIsBSTDelete _ _ = EmptyIsBSTT
instance (o ~ CmpNat x n,
  ProofIsBSTDelete' x ('ForkTree l (Node n a1) r) o) =>
  ProofIsBSTDelete x ('ForkTree l (Node n a1) r) where
  proofIsBSTDelete px (BST _ tIsBST) = proofIsBSTDelete' px tIsBST (Proxy::Proxy o)

-- | Prove that deleting a node with key 'x'
-- | in a BST tree preserves BST condition, given that the comparison between
-- | 'x' and the root key of the tree equals 'o'.
-- | The BST invariant was already check when proofIsBSTDelete was called before.
-- | The 'o' parameter guides the proof.
class ProofIsBSTDelete' (x :: Nat) (t :: Tree) (o :: Ordering) where
  proofIsBSTDelete' :: Proxy x -> IsBSTT t -> Proxy o -> IsBSTT (Delete' x t o)
instance ProofIsBSTDelete' x ('ForkTree 'EmptyTree (Node n a1) 'EmptyTree) 'EQ where
  proofIsBSTDelete' _ _ _ = EmptyIsBSTT
instance ProofIsBSTDelete' x ('ForkTree 'EmptyTree (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  proofIsBSTDelete' _ (ForkIsBSTT _ _ rIsBST@ForkIsBSTT{}) _ =  rIsBST
instance ProofIsBSTDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) 'EmptyTree) 'EQ where
  proofIsBSTDelete' _ (ForkIsBSTT lIsBST _ _) _ = lIsBST
instance (l ~ 'ForkTree ll (Node ln la) lr, r ~ 'ForkTree rl (Node rn ra) rr,
  LtN (MaxKeyDelete l) (MaxKey l) ~ 'True, GtN r (MaxKey l) ~ 'True,
  ProofMaxKeyDeleteIsBST l, ProofLtNMaxKeyDelete l n) =>
  ProofIsBSTDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  proofIsBSTDelete' _ (ForkIsBSTT l _ r) _ =
    gcastWith (proofLtNMaxKeyDelete l (Proxy::Proxy n)) $
    ForkIsBSTT (proofMaxKeyDeleteIsBST l) node r
      where
        node = undefined::Node (MaxKey l) (MaxValue l)
instance ProofIsBSTDelete' x ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  proofIsBSTDelete' _ tIsBST _ = tIsBST
instance (l ~ 'ForkTree ll (Node ln la) lr, o ~ CmpNat x ln,
  ProofIsBSTDelete' x l o, ProofLtNDelete' x l n o) =>
  ProofIsBSTDelete' x ('ForkTree l (Node n a1) r) 'LT where
  proofIsBSTDelete' px (ForkIsBSTT l node r) _ =
    gcastWith (proofLtNDelete' px l (Proxy::Proxy n) po) $
    ForkIsBSTT (proofIsBSTDelete' px l po) node r
      where
        po = Proxy::Proxy o
instance ProofIsBSTDelete' x ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  proofIsBSTDelete' _ tIsBST _ = tIsBST
instance (r ~ 'ForkTree rl (Node rn ra) rr, o ~ CmpNat x rn,
  ProofIsBSTDelete' x r o, ProofGtNDelete' x r n o) =>
  ProofIsBSTDelete' x ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'GT where
  proofIsBSTDelete' px (ForkIsBSTT l node r) _ =
    gcastWith (proofGtNDelete' px r (Proxy::Proxy n) po) $
    ForkIsBSTT l node (proofIsBSTDelete' px r po)
      where
        po = Proxy::Proxy o

-- | Prove that deleting a node with key 'x'
-- | in a tree 't' which verifies 'LtN t n ~ 'True' preserves the LtN invariant,
-- | given that the comparison between 'x' and the root key of the tree equals 'o'.
-- | The 'o' parameter guides the proof.
class ProofLtNDelete' (x :: Nat) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofLtNDelete' :: (LtN t n ~ 'True) =>
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
  LtN l n ~ 'True, LtN r n ~ 'True,
  ProofLTMaxKey l n, ProofLtNMaxKeyDelete l n) =>
  ProofLtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ where
  proofLtNDelete' _ (ForkIsBSTT l _ _) pn _ =
    gcastWith (proofLTMaxKey l pn) $
    gcastWith (proofLtNMaxKeyDelete l pn) Refl
instance ProofLtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofLtNDelete' _ _ _ _ = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, o ~ CmpNat x ln,
  LtN l n ~ 'True,
  ProofLtNDelete' x l n o) =>
  ProofLtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) r) n 'LT where
  proofLtNDelete' px (ForkIsBSTT l _ _) pn _ =
    gcastWith (proofLtNDelete' px l pn (Proxy::Proxy o)) Refl
instance ProofLtNDelete' x ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofLtNDelete' _ _ _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, o ~ CmpNat x rn,
  LtN r n ~ 'True,
  ProofLtNDelete' x r n o) =>
  ProofLtNDelete' x ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'GT where
  proofLtNDelete' px (ForkIsBSTT _ _ r) pn _ =
    gcastWith (proofLtNDelete' px r pn (Proxy::Proxy o)) Refl


-- | Prove that deleting a node with key 'x'
-- | in a tree 't' which verifies 'GtN t n ~ 'True' preserves the GtN invariant,
-- | given that the comparison between 'x' and the root key of the tree equals 'o'.
-- | The 'o' parameter guides the proof.
class ProofGtNDelete' (x :: Nat) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofGtNDelete' :: (GtN t n ~ 'True) =>
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
  GtN l n ~ 'True, GtN r n ~ 'True,
  ProofGTMaxKey l n, ProofGtNMaxKeyDelete l n) =>
  ProofGtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) r) n 'EQ where
  proofGtNDelete' _ (ForkIsBSTT l _ _) pn _ =
    gcastWith (proofGtNMaxKeyDelete l pn) $
    gcastWith (proofGTMaxKey l pn) Refl
instance ProofGtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofGtNDelete' _ _ _ _ = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, o ~ CmpNat x ln,
  GtN l n ~ 'True,
  ProofGtNDelete' x l n o) =>
  ProofGtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) r) n 'LT where
  proofGtNDelete' px (ForkIsBSTT l _ _) pn _ =
    gcastWith (proofGtNDelete' px l pn (Proxy::Proxy o)) Refl
instance ProofGtNDelete' x ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofGtNDelete' _ _ _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, o ~ CmpNat x rn,
  GtN r n ~ 'True,
  ProofGtNDelete' x r n o) =>
  ProofGtNDelete' x ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'GT where
  proofGtNDelete' px (ForkIsBSTT _ _ r) pn _ =
    gcastWith (proofGtNDelete' px r pn (Proxy::Proxy o)) Refl


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
  ProofMaxKeyDeleteIsBST r, ProofGtNMaxKeyDelete r n) =>
  ProofMaxKeyDeleteIsBST ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) ('ForkTree rl (Node rn ra) rr)) where
  proofMaxKeyDeleteIsBST (ForkIsBSTT l node r) =
    gcastWith (proofGtNMaxKeyDelete r (Proxy::Proxy n)) $
    ForkIsBSTT l node (proofMaxKeyDeleteIsBST r)

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
  proofGTMaxKey (ForkIsBSTT _ _ r) pn =
    gcastWith (proofGTMaxKey r pn) Refl

-- | Prove that in a tree 't' which verifies that 'GtN t n ~ 'True',
-- | the tree resulting from the removal of the maximum key of 't' preserves the GtN invariant.
-- | This proof is needed for the delete operation.
class ProofGtNMaxKeyDelete (t :: Tree) (n :: Nat) where
  proofGtNMaxKeyDelete :: (GtN t n ~ 'True) =>
    IsBSTT t -> Proxy n -> GtN (MaxKeyDelete t) n :~: 'True
instance (GtN l n ~ 'True) =>
  ProofGtNMaxKeyDelete ('ForkTree l (Node n1 a) 'EmptyTree) n where
  proofGtNMaxKeyDelete _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr,
  GtN r n ~ 'True,
  ProofGtNMaxKeyDelete r n) =>
  ProofGtNMaxKeyDelete ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofGtNMaxKeyDelete (ForkIsBSTT _ _ r) pn =
    gcastWith (proofGtNMaxKeyDelete r pn) Refl

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
  proofLTMaxKey (ForkIsBSTT _ _ r) pn =
    gcastWith (proofLTMaxKey r pn) Refl

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
  ProofLtNMaxKeyDelete r n) =>
  ProofLtNMaxKeyDelete ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofLtNMaxKeyDelete (ForkIsBSTT _ _ r) pn =
    gcastWith (proofLtNMaxKeyDelete r pn) Refl
