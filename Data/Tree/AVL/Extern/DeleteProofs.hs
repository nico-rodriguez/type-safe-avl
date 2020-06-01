{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Tree.AVL.Extern.DeleteProofs (
  AVL(AVL),
  IsAVL,
  ProofIsAVLDelete(proofIsAVLDelete),
  ProofIsBSTDelete(proofIsBSTDelete)
) where

import           Data.Proxy                         (Proxy (Proxy))
import           Data.Tree.AVL.Extern.BalanceProofs (ProofGtNBalance (proofGtNBalance),
                                                     ProofIsAVLBalance (proofIsAVLBalance),
                                                     ProofIsBSTBalance (proofIsBSTBalance),
                                                     ProofLtNBalance (proofLtNBalance))
import           Data.Tree.AVL.Extern.Constructor   (AVL (AVL))
import           Data.Tree.AVL.Extern.Delete        (Deletable (Delete),
                                                     Deletable' (Delete'),
                                                     MaxKeyDeletable (MaxKeyDelete))
import           Data.Tree.AVL.Invariants           (BalancedHeights, Height,
                                                     IsAVL)
import           Data.Tree.BST.Extern.Constructor   (BST (BST))
import           Data.Tree.BST.Extern.Delete        (MaxKey, MaxValue, Maxable)
import           Data.Tree.BST.Invariants           (GtN, IsBST, LtN)
import           Data.Tree.ITree                    (ITree (EmptyITree, ForkITree),
                                                     Tree (EmptyTree, ForkTree))
import           Data.Tree.Node                     (Node (Node))
import           Data.Type.Bool                     (If)
import           Data.Type.Equality                 ((:~:) (Refl), gcastWith)
import           GHC.TypeNats                       (type (+), type (<=?),
                                                     CmpNat, Nat)
import           Prelude                            (Bool (True),
                                                     Ordering (EQ, GT, LT), ($))


-- | Prove that deleting a node with key 'x'
-- | in an AVL tree preserves the AVL condition.
class ProofIsBSTDelete (x :: Nat) (t :: Tree) where
  proofIsBSTDelete :: (IsBST t ~ 'True) =>
    Proxy x -> BST t -> IsBST (Delete x t) :~: 'True
instance ProofIsBSTDelete x 'EmptyTree where
  proofIsBSTDelete _ (BST EmptyITree) = Refl
instance ProofIsBSTDelete' x ('ForkTree l (Node n a1) r) (CmpNat x n) =>
  ProofIsBSTDelete x ('ForkTree l (Node n a1) r) where
  proofIsBSTDelete px (BST t@ForkITree{}) = proofIsBSTDelete' px t (Proxy::Proxy (CmpNat x n))

-- | Prove that inserting a node with key 'x' and element value 'a'
-- | in an AVL tree preserves the AVL condition, given that the comparison between
-- | 'x' and the root key of the tree equals 'o'.
-- | The AVL invariant was already check when proofIsBSTInsert was called before.
-- | The 'o' parameter guides the proof.
class ProofIsBSTDelete' (x :: Nat) (t :: Tree) (o :: Ordering) where
  proofIsBSTDelete' :: (IsBST t ~ 'True) =>
    Proxy x -> ITree t -> Proxy o -> IsBST (Delete' x t o) :~: 'True
instance ProofIsBSTDelete' x ('ForkTree 'EmptyTree (Node n a1) 'EmptyTree) 'EQ where
  proofIsBSTDelete' _ (ForkITree EmptyITree (Node _) EmptyITree) _ = Refl
instance (IsBST rl ~ 'True, IsBST rr ~ 'True, LtN rl rn ~ 'True, GtN rr rn ~ 'True) =>
  ProofIsBSTDelete' x ('ForkTree 'EmptyTree (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  proofIsBSTDelete' _ (ForkITree EmptyITree (Node _) ForkITree{}) _ = Refl
instance (IsBST ll ~ 'True, IsBST lr ~ 'True, LtN ll ln ~ 'True, GtN lr ln ~ 'True) =>
  ProofIsBSTDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) 'EmptyTree) 'EQ where
  proofIsBSTDelete' _ (ForkITree ForkITree{} (Node _) EmptyITree) _ = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, IsBST l ~ 'True, MaxKeyDeletable l,
  r ~ 'ForkTree rl (Node rn ra) rr, IsBST r ~ 'True, LtN (MaxKeyDelete l) (MaxKey l) ~ 'True, GtN r (MaxKey l) ~ 'True,
  ProofIsBSTBalance ('ForkTree (MaxKeyDelete l) (Node (MaxKey l) (MaxValue l)) r),
  ProofMaxKeyDeleteIsBST l) =>
  ProofIsBSTDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  proofIsBSTDelete' _ (ForkITree l@ForkITree{} (Node _) ForkITree{}) _ =
    gcastWith (proofMaxKeyDeleteIsBST l) $
      gcastWith (proofIsBSTBalance (Proxy::Proxy ('ForkTree (MaxKeyDelete l) (Node (MaxKey l) (MaxValue l)) r))) Refl
instance ProofIsBSTDelete' x ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  proofIsBSTDelete' _ (ForkITree EmptyITree (Node _) _) _ = Refl
instance (IsBST r ~ 'True, GtN r n ~ 'True, LtN ('ForkTree ll (Node ln la) lr) n ~ 'True, ProofIsBSTDelete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln),
  CmpNat x n ~ 'LT, ProofLtNDelete' x ('ForkTree ll (Node ln la) lr) n (CmpNat x ln),
  ProofIsBSTBalance ('ForkTree (Delete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln)) (Node n a1) r)) =>
  ProofIsBSTDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) r) 'LT where
  proofIsBSTDelete' px (ForkITree l@ForkITree{} _ _) _ =
    gcastWith (proofLtNDelete' px l (Proxy::Proxy n) (Proxy::Proxy (CmpNat x ln))) $
      gcastWith (proofIsBSTDelete' px l (Proxy::Proxy (CmpNat x ln))) $
        gcastWith (proofIsBSTBalance (Proxy::Proxy ('ForkTree (Delete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln)) (Node n a1) r))) Refl
instance ProofIsBSTDelete' x ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  proofIsBSTDelete' _ (ForkITree _ (Node _) EmptyITree) _ = Refl
instance (IsBST l ~ 'True, LtN l n ~ 'True, ProofIsBSTDelete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn),
  CmpNat x n ~ 'GT, GtN ('ForkTree rl (Node rn ra) rr) n ~ 'True, ProofGtNDelete' x ('ForkTree rl (Node rn ra) rr) n (CmpNat x rn),
  ProofIsBSTBalance ('ForkTree l (Node n a1) (Delete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn)))) =>
  ProofIsBSTDelete' x ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'GT where
  proofIsBSTDelete' px (ForkITree _ (Node _) r@ForkITree{}) _ =
    gcastWith (proofGtNDelete' px r (Proxy::Proxy n) (Proxy::Proxy (CmpNat x rn))) $
      gcastWith (proofIsBSTDelete' px r (Proxy::Proxy (CmpNat x rn))) $
        gcastWith (proofIsBSTBalance (Proxy::Proxy ('ForkTree l (Node n a1) (Delete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn))))) Refl


-- | Prove that deleting a node with key 'x' (lower than 'n')
-- | in a tree 't' which verifies 'LtN t n ~ 'True' preserves the LtN invariant,
-- | given that the comparison between 'x' and the root key of the tree equals 'o'.
-- | The 'o' parameter guides the proof.
class ProofLtNDelete' (x :: Nat) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofLtNDelete' :: (CmpNat x n ~ 'LT, LtN t n ~ 'True) =>
    Proxy x -> ITree t -> Proxy n -> Proxy o -> LtN (Delete' x t o) n :~: 'True
instance ProofLtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) 'EmptyTree) n 'EQ where
  proofLtNDelete' _ (ForkITree EmptyITree (Node _) EmptyITree) _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, LtN r n ~ 'True) =>
  ProofLtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ where
  proofLtNDelete' _ (ForkITree EmptyITree (Node _) ForkITree{}) _ _ = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, LtN l n ~ 'True) =>
  ProofLtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) 'EmptyTree) n 'EQ where
  proofLtNDelete' _ (ForkITree ForkITree{} (Node _) EmptyITree) _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, LtN r n ~ 'True,
  l ~ 'ForkTree ll (Node ln la) lr, LtN l n ~ 'True, ProofLTMaxKey l n, Maxable l,
  ProofLtNMaxKeyDelete l n, MaxKeyDeletable l,
  ProofLtNBalance ('ForkTree (MaxKeyDelete l) (Node (MaxKey l) (MaxValue l)) r) n) =>
  ProofLtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ where
  proofLtNDelete' _ (ForkITree l@ForkITree{} (Node _) ForkITree{}) _ _ =
    gcastWith (proofLtNMaxKeyDelete l (Proxy::Proxy n)) $
      gcastWith (proofLTMaxKey l (Proxy::Proxy n)) $
        gcastWith (proofLtNBalance (Proxy::Proxy ('ForkTree (MaxKeyDelete l) (Node (MaxKey l) (MaxValue l)) r)) (Proxy::Proxy n)) Refl
instance ProofLtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofLtNDelete' _ (ForkITree EmptyITree (Node _) _) _ _ = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, LtN l n ~ 'True,
  ProofLtNDelete' x l n (CmpNat x ln),
  ProofLtNBalance ('ForkTree (Delete' x l (CmpNat x ln)) (Node n1 a1) r) n) =>
  ProofLtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) r) n 'LT where
  proofLtNDelete' px (ForkITree l@ForkITree{} _ _) _ _ =
    gcastWith (proofLtNDelete' px l (Proxy::Proxy n) (Proxy::Proxy (CmpNat x ln))) $
      gcastWith (proofLtNBalance (Proxy::Proxy ('ForkTree (Delete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln)) (Node n1 a1) r)) (Proxy::Proxy n)) Refl
instance ProofLtNDelete' x ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofLtNDelete' _ (ForkITree _ (Node _) EmptyITree) _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, LtN r n ~ 'True,
  ProofLtNDelete' x r n (CmpNat x rn),
  ProofLtNBalance ('ForkTree l (Node n1 a1) (Delete' x r (CmpNat x rn))) n) =>
  ProofLtNDelete' x ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'GT where
  proofLtNDelete' px (ForkITree _ (Node _) r@ForkITree{}) _ _ =
    gcastWith (proofLtNDelete' px r (Proxy::Proxy n) (Proxy::Proxy (CmpNat x rn))) $
      gcastWith (proofLtNBalance (Proxy::Proxy ('ForkTree l (Node n1 a1) (Delete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn)))) (Proxy::Proxy n)) Refl


-- | Prove that deleting a node with key 'x' (greater than 'n')
-- | in a tree 't' which verifies 'GtN t n ~ 'True' preserves the GtN invariant,
-- | given that the comparison between 'x' and the root key of the tree equals 'o'.
-- | The 'o' parameter guides the proof.
class ProofGtNDelete' (x :: Nat) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofGtNDelete' :: (CmpNat x n ~ 'GT, GtN t n ~ 'True) =>
    Proxy x -> ITree t -> Proxy n -> Proxy o -> GtN (Delete' x t o) n :~: 'True
instance ProofGtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) 'EmptyTree) n 'EQ where
  proofGtNDelete' _ (ForkITree EmptyITree (Node _) EmptyITree) _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, GtN r n ~ 'True) =>
  ProofGtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ where
  proofGtNDelete' _ (ForkITree EmptyITree (Node _) ForkITree{}) _ _ = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, GtN l n ~ 'True) =>
  ProofGtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) 'EmptyTree) n 'EQ where
  proofGtNDelete' _ (ForkITree ForkITree{} (Node _) EmptyITree) _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, GtN r n ~ 'True,
  l ~ 'ForkTree ll (Node ln la) lr, GtN l n ~ 'True,
  ProofGTMaxKey l n, Maxable l, ProofGtNMaxKeyDelete l n, MaxKeyDeletable l,
  ProofGtNBalance ('ForkTree (MaxKeyDelete l) (Node (MaxKey l) (MaxValue l)) r) n) =>
  ProofGtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ where
  proofGtNDelete' _ (ForkITree l@ForkITree{} (Node _) ForkITree{}) _ _ =
    gcastWith (proofGtNMaxKeyDelete l (Proxy::Proxy n)) $
      gcastWith (proofGTMaxKey l (Proxy::Proxy n)) $
        gcastWith (proofGtNBalance (Proxy::Proxy ('ForkTree (MaxKeyDelete l) (Node (MaxKey l) (MaxValue l)) r)) (Proxy::Proxy n)) Refl
instance ProofGtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofGtNDelete' _ (ForkITree EmptyITree (Node _) _) _ _ = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, GtN l n ~ 'True,
  ProofGtNDelete' x l n (CmpNat x ln),
  ProofGtNBalance ('ForkTree (Delete' x l (CmpNat x ln)) (Node n1 a1) r) n) =>
  ProofGtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) r) n 'LT where
  proofGtNDelete' px (ForkITree l@ForkITree{} _ _) _ _ =
    gcastWith (proofGtNDelete' px l (Proxy::Proxy n) (Proxy::Proxy (CmpNat x ln))) $
      gcastWith (proofGtNBalance (Proxy::Proxy ('ForkTree (Delete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln)) (Node n1 a1) r)) (Proxy::Proxy n)) Refl
instance ProofGtNDelete' x ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofGtNDelete' _ (ForkITree _ (Node _) EmptyITree) _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, GtN ('ForkTree rl (Node rn ra) rr) n ~ 'True,
  ProofGtNDelete' x r n (CmpNat x rn),
  ProofGtNBalance ('ForkTree l (Node n1 a1) (Delete' x r (CmpNat x rn))) n) =>
  ProofGtNDelete' x ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'GT where
  proofGtNDelete' px (ForkITree _ (Node _) r@ForkITree{}) _ _ =
    gcastWith (proofGtNDelete' px r (Proxy::Proxy n) (Proxy::Proxy (CmpNat x rn))) $
      gcastWith (proofGtNBalance (Proxy::Proxy ('ForkTree l (Node n1 a1) (Delete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn)))) (Proxy::Proxy n)) Refl


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
instance (l ~ 'ForkTree ll (Node ln la) lr, IsBST l ~ 'True) =>
  ProofMaxKeyDeleteIsBST ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) 'EmptyTree) where
  proofMaxKeyDeleteIsBST (ForkITree ForkITree{} (Node _) EmptyITree) = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, IsBST l ~ 'True, LtN l n ~ 'True,
  r ~ 'ForkTree rl (Node rn ra) rr, IsBST r ~ 'True,
  ProofMaxKeyDeleteIsBST r, MaxKeyDeletable r, ProofGtNMaxKeyDelete r n,
  ProofIsBSTBalance ('ForkTree l (Node n a) (MaxKeyDelete r))) =>
  ProofMaxKeyDeleteIsBST ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) ('ForkTree rl (Node rn ra) rr)) where
  proofMaxKeyDeleteIsBST (ForkITree ForkITree{} (Node _) r@ForkITree{}) =
    gcastWith (proofGtNMaxKeyDelete r (Proxy::Proxy n)) $
      gcastWith (proofMaxKeyDeleteIsBST r) $
        gcastWith (proofIsBSTBalance (Proxy::Proxy ('ForkTree l (Node n a) (MaxKeyDelete r)))) Refl


-- | Prove that in a tree 't' which verifies that 'GtN t n ~ 'True',
-- | the maximum key of 't' is also greater than 'n'.
-- | This proof is needed for the delete operation.
class ProofGTMaxKey (t :: Tree) (n :: Nat) where
  proofGTMaxKey :: (Maxable t, GtN t n ~ 'True) =>
    ITree t -> Proxy n -> CmpNat (MaxKey t) n :~: 'GT
instance (CmpNat n1 n ~ 'GT) =>
  ProofGTMaxKey ('ForkTree l (Node n1 a) 'EmptyTree) n where
  proofGTMaxKey (ForkITree _ (Node _) EmptyITree) _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, GtN r n ~ 'True, Maxable r, ProofGTMaxKey r n) =>
  ProofGTMaxKey ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofGTMaxKey (ForkITree _ (Node _) r@ForkITree{}) pn = gcastWith (proofGTMaxKey r pn) Refl

-- | Prove that in a tree 't' which verifies that 'GtN t n ~ 'True',
-- | the tree resulting from the removal of the maximum key of 't' preserves the GtN invariant.
-- | This proof is needed for the delete operation.
class ProofGtNMaxKeyDelete (t :: Tree) (n :: Nat) where
  proofGtNMaxKeyDelete :: (MaxKeyDeletable t, GtN t n ~ 'True) =>
    ITree t -> Proxy n -> GtN (MaxKeyDelete t) n :~: 'True
instance (GtN l n ~ 'True) =>
  ProofGtNMaxKeyDelete ('ForkTree l (Node n1 a) 'EmptyTree) n where
  proofGtNMaxKeyDelete (ForkITree _ (Node _) EmptyITree) _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, GtN r n ~ 'True,
  ProofGtNMaxKeyDelete r n, MaxKeyDeletable r,
  ProofGtNBalance ('ForkTree l (Node n1 a) (MaxKeyDelete r)) n) =>
  ProofGtNMaxKeyDelete ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofGtNMaxKeyDelete (ForkITree _ (Node _) r@ForkITree{}) pn =
    gcastWith (proofGtNMaxKeyDelete r pn) $
      gcastWith (proofGtNBalance (Proxy::Proxy ('ForkTree l (Node n1 a) (MaxKeyDelete r))) (Proxy::Proxy n)) Refl

-- | Prove that in a tree 't' which verifies that 'LtN t n ~ 'True',
-- | the maximum key of 't' is also less than 'n'.
-- | This proof is needed for the delete operation.
class ProofLTMaxKey (t :: Tree) (n :: Nat) where
  proofLTMaxKey :: (Maxable t, LtN t n ~ 'True) =>
    ITree t -> Proxy n -> CmpNat (MaxKey t) n :~: 'LT
instance (CmpNat n1 n ~ 'LT) =>
  ProofLTMaxKey ('ForkTree l (Node n1 a) 'EmptyTree) n where
  proofLTMaxKey (ForkITree _ (Node _) EmptyITree) _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, LtN r n ~ 'True, Maxable r, ProofLTMaxKey r n) =>
  ProofLTMaxKey ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofLTMaxKey (ForkITree _ (Node _) r@ForkITree{}) pn = gcastWith (proofLTMaxKey r pn) Refl

-- | Prove that in a tree 't' which verifies that 'LtN t n ~ 'True',
-- | the tree resulting from the removal of the maximum key of 't' preserves the LtN invariant.
-- | This proof is needed for the delete operation.
class ProofLtNMaxKeyDelete (t :: Tree) (n :: Nat) where
  proofLtNMaxKeyDelete :: (MaxKeyDeletable t, LtN t n ~ 'True) =>
    ITree t -> Proxy n -> LtN (MaxKeyDelete t) n :~: 'True
instance (LtN l n ~ 'True) =>
  ProofLtNMaxKeyDelete ('ForkTree l (Node n1 a) 'EmptyTree) n where
  proofLtNMaxKeyDelete (ForkITree _ (Node _) EmptyITree) _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, LtN r n ~ 'True,
  ProofLtNMaxKeyDelete r n, MaxKeyDeletable r,
  ProofLtNBalance ('ForkTree l (Node n1 a) (MaxKeyDelete r)) n) =>
  ProofLtNMaxKeyDelete ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofLtNMaxKeyDelete (ForkITree _ (Node _) r@ForkITree{}) pn =
    gcastWith (proofLtNMaxKeyDelete r pn) $
      gcastWith (proofLtNBalance (Proxy::Proxy ('ForkTree l (Node n1 a) (MaxKeyDelete r))) (Proxy::Proxy n)) Refl


-- | Prove that deleting the node with maximum key value
-- | in a BST 't' preserves the BST invariant.
-- | This proof is needed for the delete operation.
class ProofMaxKeyDeleteIsAVL (t :: Tree) where
  proofMaxKeyDeleteIsAVL :: (IsAVL t ~ 'True, MaxKeyDeletable t) =>
    ITree t -> IsAVL (MaxKeyDelete t) :~: 'True
instance ProofMaxKeyDeleteIsAVL 'EmptyTree where
  proofMaxKeyDeleteIsAVL EmptyITree = Refl
instance ProofMaxKeyDeleteIsAVL ('ForkTree 'EmptyTree (Node n a) 'EmptyTree) where
  proofMaxKeyDeleteIsAVL (ForkITree EmptyITree (Node _) EmptyITree) = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, IsAVL l ~ 'True) =>
  ProofMaxKeyDeleteIsAVL ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) 'EmptyTree) where
  proofMaxKeyDeleteIsAVL (ForkITree ForkITree{} (Node _) EmptyITree) = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, IsAVL l ~ 'True, LtN l n ~ 'True,
  r ~ 'ForkTree rl (Node rn ra) rr, IsAVL r ~ 'True,
  BalancedHeights (1 + If (Height ll <=? Height lr) (Height lr) (Height ll)) (Height (MaxKeyDelete ('ForkTree rl (Node rn ra) rr))) ~ 'True,
  ProofMaxKeyDeleteIsAVL r, MaxKeyDeletable r,
  ProofIsAVLBalance ('ForkTree l (Node n a) (MaxKeyDelete r))) =>
  ProofMaxKeyDeleteIsAVL ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) ('ForkTree rl (Node rn ra) rr)) where
  proofMaxKeyDeleteIsAVL (ForkITree ForkITree{} (Node _) r@ForkITree{}) =
    gcastWith (proofMaxKeyDeleteIsAVL r) $
      gcastWith (proofIsAVLBalance (Proxy::Proxy ('ForkTree l (Node n a) (MaxKeyDelete r)))) Refl


-- | Prove that deleting a node with key 'x'
-- | in an AVL tree preserves the AVL condition.
class ProofIsAVLDelete (x :: Nat) (t :: Tree) where
  proofIsAVLDelete :: Proxy x -> AVL t -> IsAVL (Delete x t) :~: 'True
instance ProofIsAVLDelete x 'EmptyTree where
  proofIsAVLDelete _ (AVL EmptyITree) = Refl
instance ProofIsAVLDelete' x ('ForkTree l (Node n a1) r) (CmpNat x n) =>
  ProofIsAVLDelete x ('ForkTree l (Node n a1) r) where
  proofIsAVLDelete px (AVL t) = proofIsAVLDelete' px t (Proxy::Proxy (CmpNat x n))

-- | Prove that deleting a node with key 'x'
-- | in an AVL tree preserves the AVL condition, given that the comparison between
-- | 'x' and the root key of the tree equals 'o'.
-- | The AVL invariant was already check when proofIsBSTDelete was called before.
-- | The 'o' parameter guides the proof.
class ProofIsAVLDelete' (x :: Nat) (t :: Tree) (o :: Ordering) where
  proofIsAVLDelete' :: (IsAVL t ~ 'True) =>
    Proxy x -> ITree t -> Proxy o -> IsAVL (Delete' x t o) :~: 'True
instance ProofIsAVLDelete' x ('ForkTree 'EmptyTree (Node n a1) 'EmptyTree) 'EQ where
  proofIsAVLDelete' _ (ForkITree EmptyITree (Node _) EmptyITree) _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, IsAVL r ~ 'True) =>
  ProofIsAVLDelete' x ('ForkTree 'EmptyTree (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  proofIsAVLDelete' _ (ForkITree EmptyITree (Node _) ForkITree{}) _ = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, IsAVL l ~ 'True) =>
  ProofIsAVLDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) 'EmptyTree) 'EQ where
  proofIsAVLDelete' _ (ForkITree ForkITree{} (Node _) EmptyITree) _ = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, IsAVL (MaxKeyDelete l) ~ 'True,
  r ~ 'ForkTree rl (Node rn ra) rr,
  ProofIsAVLBalance ('ForkTree (MaxKeyDelete l) (Node (MaxKey l) (MaxValue l)) r)) =>
  ProofIsAVLDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  proofIsAVLDelete' _ (ForkITree ForkITree{} (Node _) ForkITree{}) _ =
    gcastWith (proofIsAVLBalance (Proxy::Proxy ('ForkTree (MaxKeyDelete l) (Node (MaxKey l) (MaxValue l)) r))) Refl
instance ProofIsAVLDelete' x ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  proofIsAVLDelete' _ (ForkITree EmptyITree (Node _) _) _ = Refl
instance (ProofIsAVLBalance ('ForkTree (Delete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln)) (Node n a1) r)) =>
  ProofIsAVLDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) r) 'LT where
  proofIsAVLDelete' _ (ForkITree ForkITree{} _ _) _ =
    gcastWith (proofIsAVLBalance (Proxy::Proxy ('ForkTree (Delete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln)) (Node n a1) r))) Refl
instance ProofIsAVLDelete' x ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  proofIsAVLDelete' _ (ForkITree _ (Node _) EmptyITree) _ = Refl
instance (ProofIsAVLBalance ('ForkTree l (Node n a1) (Delete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn)))) =>
  ProofIsAVLDelete' x ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'GT where
  proofIsAVLDelete' _ (ForkITree _ _ ForkITree{}) _ =
    gcastWith (proofIsAVLBalance (Proxy::Proxy ('ForkTree l (Node n a1) (Delete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn))))) Refl
