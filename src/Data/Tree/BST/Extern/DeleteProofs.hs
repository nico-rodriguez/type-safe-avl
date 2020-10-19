{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Tree.BST.Extern.DeleteProofs (
  ProofIsBSTDelete(proofIsBSTDelete)
) where

import           Data.Proxy                       (Proxy (Proxy))
import           Data.Tree.BST.Extern.Constructor (BST (BST))
import           Data.Tree.BST.Extern.Delete      (Deletable (Delete),
                                                   Deletable' (Delete'),
                                                   MaxKeyDeletable (MaxKeyDelete, MaxKeyDelete),
                                                   Maxable (MaxKey))
import           Data.Tree.BST.Invariants         (GtN, IsBST, LtN)
import           Data.Tree.BST.InvariantsProofs   (proofGtNGT,
                                                   proofGtNLeftSubTree,
                                                   proofGtNRightSubTree,
                                                   proofIsBSTGtN,
                                                   proofIsBSTLeftSubTree,
                                                   proofIsBSTLtN,
                                                   proofIsBSTRightSubTree,
                                                   proofLtNLT,
                                                   proofLtNLeftSubTree,
                                                   proofLtNRightSubTree)
import           Data.Tree.ITree                  (ITree (EmptyITree, ForkITree),
                                                   Tree (EmptyTree, ForkTree))
import           Data.Tree.Node                   (Node (Node))
import           Data.Type.Equality               ((:~:) (Refl), gcastWith)
import           GHC.TypeNats                     (CmpNat, Nat)
import           Prelude                          (Bool (True),
                                                   Ordering (EQ, GT, LT), ($))


-- | Prove that deleting a node with key 'x'
-- | in a BST tree preserves BST condition.
class ProofIsBSTDelete (x :: Nat) (t :: Tree) where
  proofIsBSTDelete :: Proxy x -> BST t -> IsBST (Delete x t) :~: 'True
instance ProofIsBSTDelete x 'EmptyTree where
  proofIsBSTDelete _ (BST EmptyITree) = Refl
instance ProofIsBSTDelete' x ('ForkTree l (Node n a1) r) (CmpNat x n) =>
  ProofIsBSTDelete x ('ForkTree l (Node n a1) r) where
  proofIsBSTDelete px (BST t@ForkITree{}) = proofIsBSTDelete' px t (Proxy::Proxy (CmpNat x n))

-- | Prove that deleting a node with key 'x'
-- | in a BST tree preserves BST condition, given that the comparison between
-- | 'x' and the root key of the tree equals 'o'.
-- | The BST invariant was already check when proofIsBSTDelete was called before.
-- | The 'o' parameter guides the proof.
class ProofIsBSTDelete' (x :: Nat) (t :: Tree) (o :: Ordering) where
  proofIsBSTDelete' :: (IsBST t ~ 'True) =>
    Proxy x -> ITree t -> Proxy o -> IsBST (Delete' x t o) :~: 'True
instance ProofIsBSTDelete' x ('ForkTree 'EmptyTree (Node n a1) 'EmptyTree) 'EQ where
  proofIsBSTDelete' _ (ForkITree EmptyITree (Node _) EmptyITree) _ = Refl
instance ProofIsBSTDelete' x ('ForkTree 'EmptyTree (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  proofIsBSTDelete' _ (ForkITree EmptyITree (Node _) ForkITree{}) _ =
    gcastWith (proofIsBSTRightSubTree (Proxy::Proxy ('ForkTree 'EmptyTree (Node n a1) ('ForkTree rl (Node rn ra) rr))) Refl) Refl
instance ProofIsBSTDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) 'EmptyTree) 'EQ where
  proofIsBSTDelete' _ (ForkITree ForkITree{} (Node _) EmptyITree) _ =
    gcastWith (proofIsBSTLeftSubTree (Proxy::Proxy ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) 'EmptyTree)) Refl) Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, MaxKeyDeletable l, LtN (MaxKeyDelete l) (MaxKey l) ~ 'True,
  r ~ 'ForkTree rl (Node rn ra) rr, GtN r (MaxKey l) ~ 'True, ProofMaxKeyDeleteIsBST l) =>
  ProofIsBSTDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  proofIsBSTDelete' _ (ForkITree l@ForkITree{} (Node _) ForkITree{}) _ =
    gcastWith (proofIsBSTRightSubTree (Proxy::Proxy ('ForkTree l (Node n a1) r)) Refl) $
    gcastWith (proofIsBSTLeftSubTree (Proxy::Proxy ('ForkTree l (Node n a1) r)) Refl) $
    gcastWith (proofMaxKeyDeleteIsBST l) Refl
instance ProofIsBSTDelete' x ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  proofIsBSTDelete' _ (ForkITree EmptyITree (Node _) _) _ = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, ProofIsBSTDelete' x l (CmpNat x ln), ProofLtNDelete' x l n (CmpNat x ln)) =>
  ProofIsBSTDelete' x ('ForkTree l (Node n a1) r) 'LT where
  proofIsBSTDelete' px (ForkITree l@ForkITree{} _ _) _ =
    gcastWith (proofIsBSTLtN (Proxy::Proxy ('ForkTree l (Node n a) r)) Refl) $
    gcastWith (proofLtNDelete' px l (Proxy::Proxy n) (Proxy::Proxy (CmpNat x ln))) $
    gcastWith (proofIsBSTGtN (Proxy::Proxy ('ForkTree l (Node n a) r)) Refl) $
    gcastWith (proofIsBSTRightSubTree (Proxy::Proxy ('ForkTree l (Node n a) r)) Refl) $
    gcastWith (proofIsBSTDelete' px l (Proxy::Proxy (CmpNat x ln))) Refl
instance ProofIsBSTDelete' x ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  proofIsBSTDelete' _ (ForkITree _ (Node _) EmptyITree) _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, ProofIsBSTDelete' x r (CmpNat x rn), ProofGtNDelete' x r n (CmpNat x rn)) =>
  ProofIsBSTDelete' x ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'GT where
  proofIsBSTDelete' px (ForkITree _ (Node _) r@ForkITree{}) _ =
    gcastWith (proofIsBSTGtN (Proxy::Proxy ('ForkTree l (Node n a) r)) Refl) $
    gcastWith (proofGtNDelete' px r (Proxy::Proxy n) (Proxy::Proxy (CmpNat x rn))) $
    gcastWith (proofIsBSTRightSubTree (Proxy::Proxy ('ForkTree l (Node n a) r)) Refl) $
    gcastWith (proofIsBSTDelete' px r (Proxy::Proxy (CmpNat x rn))) Refl

-- | Prove that deleting a node with key 'x'
-- | in a tree 't' which verifies 'LtN t n ~ 'True' preserves the LtN invariant,
-- | given that the comparison between 'x' and the root key of the tree equals 'o'.
-- | The 'o' parameter guides the proof.
class ProofLtNDelete' (x :: Nat) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofLtNDelete' :: (LtN t n ~ 'True) =>
    Proxy x -> ITree t -> Proxy n -> Proxy o -> LtN (Delete' x t o) n :~: 'True
instance ProofLtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) 'EmptyTree) n 'EQ where
  proofLtNDelete' _ (ForkITree EmptyITree (Node _) EmptyITree) _ _ = Refl
instance ProofLtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ where
  proofLtNDelete' _ (ForkITree EmptyITree (Node _) ForkITree{}) pn _ =
    gcastWith (proofLtNRightSubTree (Proxy::Proxy ('ForkTree 'EmptyTree (Node n1 a1) ('ForkTree rl (Node rn ra) rr))) pn Refl) Refl
instance ProofLtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) 'EmptyTree) n 'EQ where
  proofLtNDelete' _ (ForkITree ForkITree{} (Node _) EmptyITree) pn _ =
    gcastWith (proofLtNLeftSubTree (Proxy::Proxy ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) 'EmptyTree)) pn Refl) Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, r ~ 'ForkTree rl (Node rn ra) rr,
  ProofLTMaxKey l n, Maxable l, ProofLtNMaxKeyDelete l n, MaxKeyDeletable l) =>
  ProofLtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ where
  proofLtNDelete' _ (ForkITree l@ForkITree{} (Node _) ForkITree{}) pn _ =
    gcastWith (proofLtNLeftSubTree (Proxy::Proxy ('ForkTree l (Node n1 a1) r)) pn Refl) $
    gcastWith (proofLtNMaxKeyDelete l pn) $
    gcastWith (proofLTMaxKey l pn) $
    gcastWith (proofLtNRightSubTree (Proxy::Proxy ('ForkTree l (Node n1 a1) r)) pn Refl) Refl
instance ProofLtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofLtNDelete' _ (ForkITree EmptyITree (Node _) _) _ _ = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, ProofLtNDelete' x l n (CmpNat x ln)) =>
  ProofLtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) r) n 'LT where
  proofLtNDelete' px (ForkITree l@ForkITree{} _ _) pn _ =
    gcastWith (proofLtNLeftSubTree (Proxy::Proxy ('ForkTree l (Node n1 a1) r)) pn Refl) $
    gcastWith (proofLtNDelete' px l pn (Proxy::Proxy (CmpNat x ln))) Refl
instance ProofLtNDelete' x ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofLtNDelete' _ (ForkITree _ (Node _) EmptyITree) _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, ProofLtNDelete' x r n (CmpNat x rn)) =>
  ProofLtNDelete' x ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'GT where
  proofLtNDelete' px (ForkITree _ (Node _) r@ForkITree{}) pn _ =
    gcastWith (proofLtNRightSubTree (Proxy::Proxy ('ForkTree l (Node n1 a1) r)) pn Refl) $
    gcastWith (proofLtNDelete' px r pn (Proxy::Proxy (CmpNat x rn))) Refl


-- | Prove that deleting a node with key 'x'
-- | in a tree 't' which verifies 'GtN t n ~ 'True' preserves the GtN invariant,
-- | given that the comparison between 'x' and the root key of the tree equals 'o'.
-- | The 'o' parameter guides the proof.
class ProofGtNDelete' (x :: Nat) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofGtNDelete' :: (GtN t n ~ 'True) =>
    Proxy x -> ITree t -> Proxy n -> Proxy o -> GtN (Delete' x t o) n :~: 'True
instance ProofGtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) 'EmptyTree) n 'EQ where
  proofGtNDelete' _ (ForkITree EmptyITree (Node _) EmptyITree) _ _ = Refl
instance ProofGtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ where
  proofGtNDelete' _ (ForkITree EmptyITree (Node _) ForkITree{}) pn _ =
    gcastWith (proofGtNRightSubTree (Proxy::Proxy ('ForkTree 'EmptyTree (Node n1 a1) ('ForkTree rl (Node rn ra) rr))) pn Refl)  Refl
instance ProofGtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) 'EmptyTree) n 'EQ where
  proofGtNDelete' _ (ForkITree ForkITree{} (Node _) EmptyITree) pn _ =
    gcastWith (proofGtNLeftSubTree (Proxy::Proxy ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) 'EmptyTree)) pn Refl) Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, r ~ 'ForkTree rl (Node rn ra) rr,
  ProofGTMaxKey l n, Maxable l, ProofGtNMaxKeyDelete l n, MaxKeyDeletable l) =>
  ProofGtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) r) n 'EQ where
  proofGtNDelete' _ (ForkITree l@ForkITree{} (Node _) ForkITree{}) pn _ =
    gcastWith (proofGtNLeftSubTree (Proxy::Proxy ('ForkTree l (Node n1 a1) r)) pn Refl) $
    gcastWith (proofGtNMaxKeyDelete l pn) $
    gcastWith (proofGTMaxKey l pn) $
    gcastWith (proofGtNRightSubTree (Proxy::Proxy ('ForkTree l (Node n1 a1) r)) pn Refl) Refl
instance ProofGtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofGtNDelete' _ (ForkITree EmptyITree (Node _) _) _ _ = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, ProofGtNDelete' x l n (CmpNat x ln)) =>
  ProofGtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) r) n 'LT where
  proofGtNDelete' px (ForkITree l@ForkITree{} _ _) pn _ =
    gcastWith (proofGtNLeftSubTree (Proxy::Proxy ('ForkTree l (Node n1 a1) r)) pn Refl) $
    gcastWith (proofGtNDelete' px l pn (Proxy::Proxy (CmpNat x ln))) Refl
instance ProofGtNDelete' x ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofGtNDelete' _ (ForkITree _ (Node _) EmptyITree) _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, ProofGtNDelete' x r n (CmpNat x rn)) =>
  ProofGtNDelete' x ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'GT where
  proofGtNDelete' px (ForkITree _ (Node _) r@ForkITree{}) pn _ =
    gcastWith (proofGtNRightSubTree (Proxy::Proxy ('ForkTree l (Node n1 a1) r)) pn Refl) $
    gcastWith (proofGtNDelete' px r pn (Proxy::Proxy (CmpNat x rn))) Refl


-- | Prove that deleting the node with maximum key value
-- | in a BST 't' preserves the BST invariant.
-- | This proof is needed for the delete operation.
class ProofMaxKeyDeleteIsBST (t :: Tree) where
  proofMaxKeyDeleteIsBST :: (IsBST t ~ 'True, MaxKeyDeletable t) =>
    ITree t -> IsBST (MaxKeyDelete t) :~: 'True
instance ProofMaxKeyDeleteIsBST 'EmptyTree where
  proofMaxKeyDeleteIsBST EmptyITree = Refl
instance ProofMaxKeyDeleteIsBST ('ForkTree 'EmptyTree (Node n a) 'EmptyTree) where
  proofMaxKeyDeleteIsBST (ForkITree EmptyITree (Node _) EmptyITree) = Refl
instance ProofMaxKeyDeleteIsBST ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) 'EmptyTree) where
  proofMaxKeyDeleteIsBST (ForkITree ForkITree{} (Node _) EmptyITree) =
    gcastWith (proofIsBSTLeftSubTree (Proxy::Proxy ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) 'EmptyTree)) Refl)  Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, r ~ 'ForkTree rl (Node rn ra) rr,
  ProofMaxKeyDeleteIsBST r, MaxKeyDeletable r, ProofGtNMaxKeyDelete r n) =>
  ProofMaxKeyDeleteIsBST ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) ('ForkTree rl (Node rn ra) rr)) where
  proofMaxKeyDeleteIsBST (ForkITree ForkITree{} (Node _) r@ForkITree{}) =
    gcastWith (proofIsBSTLeftSubTree (Proxy::Proxy ('ForkTree l (Node n a) r)) Refl) $
    gcastWith (proofIsBSTLtN (Proxy::Proxy ('ForkTree l (Node n a) r)) Refl) $
    gcastWith (proofIsBSTGtN (Proxy::Proxy ('ForkTree l (Node n a) r)) Refl) $
    gcastWith (proofGtNMaxKeyDelete r (Proxy::Proxy n)) $
    gcastWith (proofMaxKeyDeleteIsBST r) Refl

-- | Prove that in a tree 't' which verifies that 'GtN t n ~ 'True',
-- | the maximum key of 't' is also greater than 'n'.
-- | This proof is needed for the delete operation.
class ProofGTMaxKey (t :: Tree) (n :: Nat) where
  proofGTMaxKey :: (Maxable t, GtN t n ~ 'True) =>
    ITree t -> Proxy n -> CmpNat (MaxKey t) n :~: 'GT
instance ProofGTMaxKey ('ForkTree l (Node n1 a) 'EmptyTree) n where
  proofGTMaxKey (ForkITree _ (Node _) EmptyITree) pn =
    gcastWith (proofGtNGT (Proxy::Proxy ('ForkTree l (Node n1 a) 'EmptyTree)) pn Refl) Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, Maxable r, ProofGTMaxKey r n) =>
  ProofGTMaxKey ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofGTMaxKey (ForkITree _ (Node _) r@ForkITree{}) pn =
    gcastWith (proofGtNRightSubTree (Proxy::Proxy ('ForkTree l (Node n1 a) r)) pn Refl) $
    gcastWith (proofGTMaxKey r pn) Refl

-- | Prove that in a tree 't' which verifies that 'GtN t n ~ 'True',
-- | the tree resulting from the removal of the maximum key of 't' preserves the GtN invariant.
-- | This proof is needed for the delete operation.
class ProofGtNMaxKeyDelete (t :: Tree) (n :: Nat) where
  proofGtNMaxKeyDelete :: (MaxKeyDeletable t, GtN t n ~ 'True) =>
    ITree t -> Proxy n -> GtN (MaxKeyDelete t) n :~: 'True
instance ProofGtNMaxKeyDelete ('ForkTree l (Node n1 a) 'EmptyTree) n where
  proofGtNMaxKeyDelete (ForkITree _ (Node _) EmptyITree) pn =
    gcastWith (proofGtNLeftSubTree (Proxy::Proxy ('ForkTree l (Node n1 a) 'EmptyTree)) pn Refl) Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, ProofGtNMaxKeyDelete r n, MaxKeyDeletable r) =>
  ProofGtNMaxKeyDelete ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofGtNMaxKeyDelete (ForkITree _ (Node _) r@ForkITree{}) pn =
    gcastWith (proofGtNRightSubTree (Proxy::Proxy ('ForkTree l (Node n1 a) r)) pn Refl) $
    gcastWith (proofGtNMaxKeyDelete r pn) Refl

-- | Prove that in a tree 't' which verifies that 'LtN t n ~ 'True',
-- | the maximum key of 't' is also less than 'n'.
-- | This proof is needed for the delete operation.
class ProofLTMaxKey (t :: Tree) (n :: Nat) where
  proofLTMaxKey :: (Maxable t, LtN t n ~ 'True) =>
    ITree t -> Proxy n -> CmpNat (MaxKey t) n :~: 'LT
instance ProofLTMaxKey ('ForkTree l (Node n1 a) 'EmptyTree) n where
  proofLTMaxKey (ForkITree _ (Node _) EmptyITree) pn =
    gcastWith (proofLtNLT (Proxy::Proxy ('ForkTree l (Node n1 a) 'EmptyTree)) pn Refl) Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, Maxable r, ProofLTMaxKey r n) =>
  ProofLTMaxKey ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofLTMaxKey (ForkITree _ (Node _) r@ForkITree{}) pn =
    gcastWith (proofLtNRightSubTree (Proxy::Proxy ('ForkTree l (Node n1 a) r)) pn Refl) $
    gcastWith (proofLTMaxKey r pn) Refl

-- | Prove that in a tree 't' which verifies that 'LtN t n ~ 'True',
-- | the tree resulting from the removal of the maximum key of 't' preserves the LtN invariant.
-- | This proof is needed for the delete operation.
class ProofLtNMaxKeyDelete (t :: Tree) (n :: Nat) where
  proofLtNMaxKeyDelete :: (MaxKeyDeletable t, LtN t n ~ 'True) =>
    ITree t -> Proxy n -> LtN (MaxKeyDelete t) n :~: 'True
instance ProofLtNMaxKeyDelete ('ForkTree l (Node n1 a) 'EmptyTree) n where
  proofLtNMaxKeyDelete (ForkITree _ (Node _) EmptyITree) pn =
    gcastWith (proofLtNLeftSubTree (Proxy::Proxy ('ForkTree l (Node n1 a) 'EmptyTree)) pn Refl) Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, MaxKeyDeletable r, ProofLtNMaxKeyDelete r n) =>
  ProofLtNMaxKeyDelete ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofLtNMaxKeyDelete (ForkITree _ (Node _) r@ForkITree{}) pn =
    gcastWith (proofLtNRightSubTree (Proxy::Proxy ('ForkTree l (Node n1 a) r)) pn Refl) $
    gcastWith (proofLtNMaxKeyDelete r pn) Refl
