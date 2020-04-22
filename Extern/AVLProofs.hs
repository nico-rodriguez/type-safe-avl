{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Extern.AVLProofs (
  AVL(AVL),
  ProofIsAVLInsert(proofIsAVLInsert),
  ProofIsAVLDelete(proofIsAVLDelete),
  ProofIsBSTInsert(proofIsBSTInsert),
  ProofIsBSTDelete(proofIsBSTDelete)
) where

import           Data.Kind            (Type)
import           Data.Proxy           (Proxy (Proxy))
import           Data.Type.Bool       (type (&&), If)
import           Data.Type.Equality   ((:~:) (Refl), gcastWith)
import           Extern.AVLOperations (BS (Balanced, LeftHeavy, RightHeavy),
                                       Balance, Balance', BalancedHeights,
                                       BalancedState, Delete, Delete', Height,
                                       Insert, Insert', Rotate,
                                       US (LeftUnbalanced, NotUnbalanced, RightUnbalanced),
                                       UnbalancedState)
import           Extern.BSTOperations (MaxKey, MaxKeyDeletable (), MaxKeyDelete,
                                       MaxValue (), Maxable)
import           Extern.BSTProofs     (GtN, IsBST, LtN, BST(BST))
import           GHC.TypeNats         (type (+), type (<=?), CmpNat, Nat)
import           ITree                (ITree (EmptyITree, ForkITree),
                                       Tree (EmptyTree, ForkTree))
import           Node                 (Node (Node))
import           Prelude              (Bool (True), Ordering (EQ, GT, LT),
                                       Show (show), ($), (++))
import           Unsafe.Coerce        (unsafeCoerce)


-- | Check if tree is AVL by comparing the differences in the heights of all sub trees pairs
-- | It doesn't check if the tree is BST, IsBST is used for that.
type family IsAVL (t :: Tree) :: Bool where
  IsAVL 'EmptyTree                   = 'True
  IsAVL ('ForkTree l (Node _n _a) r) =
    BalancedHeights (Height l) (Height r) && IsAVL l && IsAVL r


-- | Constructor of AVLs. Given an arbitrary tree, it tests wether it verifies the BST and  AVL invariant.
data AVL :: Tree -> Type where
  AVL :: (IsBST t ~ 'True, IsAVL t ~ 'True) => ITree t -> AVL t

instance Show (AVL t) where
  show (AVL t) = "AVL $ " ++ show t


-- | Prove that inserting a node with key 'x' and element value 'a'
-- | in a BST tree preserves BST condition.
class ProofIsBSTInsert (x :: Nat) (a :: Type) (t :: Tree) where
  proofIsBSTInsert :: Node x a -> BST t -> IsBST (Insert x a t) :~: 'True
instance ProofIsBSTInsert x a 'EmptyTree where
  proofIsBSTInsert _ (BST EmptyITree) = Refl
instance ProofIsBSTInsert' x a ('ForkTree l (Node n a1) r) (CmpNat x n) =>
  ProofIsBSTInsert x a ('ForkTree l (Node n a1) r) where
  proofIsBSTInsert node (BST t) = proofIsBSTInsert' node t (Proxy::Proxy (CmpNat x n))

-- | Prove that inserting a node with key 'x' and element value 'a'
-- | in a BST tree preserves BST condition, given that the comparison between
-- | 'x' and the root key of the tree equals 'o'.
-- | The BST invariant was already check when proofIsBSTInsert was called before.
-- | The 'o' parameter guides the proof.
class ProofIsBSTInsert' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) where
  proofIsBSTInsert' :: (IsBST t ~ 'True) =>
    Node x a -> ITree t -> Proxy o -> IsBST (Insert' x a t o) :~: 'True
instance (LtN l n ~ 'True, GtN r n ~ 'True, IsBST l ~ 'True, IsBST r ~ 'True) =>
  ProofIsBSTInsert' x a ('ForkTree l (Node n a1) r) 'EQ where
  proofIsBSTInsert' _ ForkITree{} _ = Refl
instance (IsBST('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n a1) r) ~ 'True,
  ProofIsBSTBalance ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n a1) r)) =>
  ProofIsBSTInsert' x a ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  proofIsBSTInsert' _ (ForkITree EmptyITree _ _) _ =
    gcastWith (proofIsBSTBalance (Proxy::Proxy ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n a1) r))) Refl
instance (l ~ 'ForkTree ll (Node ln lna) lr, CmpNat x n ~ 'LT, LtN l n ~ 'True,
  GtN r n ~ 'True, IsBST r ~ 'True,
  ProofIsBSTInsert' x a l (CmpNat x ln), ProofLtNInsert' x a l n (CmpNat x ln),
  ProofIsBSTBalance ('ForkTree (Insert' x a l (CmpNat x ln)) (Node n a1) r)) =>
  ProofIsBSTInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT where
  proofIsBSTInsert' node (ForkITree l@ForkITree{} _ _) _ =
    gcastWith (proofLtNInsert' node l (Proxy::Proxy n) (Proxy::Proxy (CmpNat x ln))) $
      gcastWith (proofIsBSTInsert' node l (Proxy::Proxy (CmpNat x ln))) $
        gcastWith (proofIsBSTBalance (Proxy::Proxy ('ForkTree (Insert' x a l (CmpNat x ln)) (Node n a1) r))) Refl
instance (CmpNat x n ~ 'GT, LtN l n ~ 'True, IsBST l ~ 'True,
  ProofIsBSTBalance ('ForkTree l (Node n a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree))) =>
  ProofIsBSTInsert' x a ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  proofIsBSTInsert' _ (ForkITree _ _ EmptyITree) _ =
    gcastWith (proofIsBSTBalance (Proxy::Proxy ('ForkTree l (Node n a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree)))) Refl
instance (LtN l n ~ 'True, IsBST l ~ 'True,
  r ~ 'ForkTree rl (Node rn rna) rr, CmpNat x n ~ 'GT, GtN r n ~ 'True,
  ProofGtNInsert' x a r n (CmpNat x rn), ProofIsBSTInsert' x a r (CmpNat x rn),
  ProofIsBSTBalance ('ForkTree l (Node n a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn)))) =>
  ProofIsBSTInsert' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT where
  proofIsBSTInsert' node (ForkITree _ _ r@ForkITree{}) _ =
    gcastWith (proofGtNInsert' node r (Proxy::Proxy n) (Proxy::Proxy (CmpNat x rn))) $
      gcastWith (proofIsBSTInsert' node r (Proxy::Proxy (CmpNat x rn))) $
        gcastWith (proofIsBSTBalance (Proxy::Proxy ('ForkTree l (Node n a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn))))) Refl


-- | Prove that inserting a node with key 'x' (lower than 'n') and element value 'a'
-- | in a tree 't' which verifies 'LtN t n ~ 'True' preserves the LtN invariant,
-- | given that the comparison between 'x' and the root key of the tree equals 'o'.
-- | The 'o' parameter guides the proof.
class ProofLtNInsert' (x :: Nat) (a :: Type) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofLtNInsert' :: (CmpNat x n ~ 'LT, LtN t n ~ 'True) =>
    Node x a -> ITree t -> Proxy n -> Proxy o -> LtN (Insert x a t) n :~: 'True
instance (CmpNat x n1 ~ 'EQ) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) r) n 'EQ where
  proofLtNInsert' _ ForkITree{} _ _ = Refl
instance (CmpNat x n1 ~ 'LT, ProofLtNBalance ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n1 a1) r) n) =>
  ProofLtNInsert' x a ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofLtNInsert' _ (ForkITree EmptyITree _ _) _ _ =
    gcastWith (proofLtNBalance (Proxy::Proxy ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n1 a1) r)) (Proxy::Proxy n)) Refl
instance (CmpNat x n1 ~ 'LT, l ~ 'ForkTree ll (Node ln lna) lr, LtN l n ~ 'True,
  ProofLtNInsert' x a l n (CmpNat x ln), ProofLtNBalance ('ForkTree (Insert' x a ('ForkTree ll (Node ln lna) lr) (CmpNat x ln)) (Node n1 a1) r) n) =>
  ProofLtNInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n1 a1) r) n 'LT where
  proofLtNInsert' node (ForkITree l@ForkITree{} _ _) n _ =
    gcastWith (proofLtNInsert' node l n (Proxy::Proxy (CmpNat x ln))) $
      gcastWith (proofLtNBalance (Proxy::Proxy ('ForkTree (Insert' x a ('ForkTree ll (Node ln lna) lr) (CmpNat x ln)) (Node n1 a1) r)) (Proxy::Proxy n)) Refl
instance (CmpNat x n1 ~ 'GT, ProofLtNBalance ('ForkTree l (Node n1 a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree)) n) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofLtNInsert' _ (ForkITree _ _ EmptyITree) _ _ =
    gcastWith (proofLtNBalance (Proxy::Proxy ('ForkTree l (Node n1 a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree))) (Proxy::Proxy n)) Refl
instance (CmpNat x n1 ~ 'GT, r ~ 'ForkTree rl (Node rn rna) rr, LtN r n ~ 'True,
  ProofLtNInsert' x a r n (CmpNat x rn),
  ProofLtNBalance ('ForkTree l (Node n1 a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn))) n) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn rna) rr)) n 'GT where
  proofLtNInsert' node (ForkITree _ _ r@ForkITree{}) n _ =
    gcastWith (proofLtNInsert' node r n (Proxy::Proxy (CmpNat x rn))) $
      gcastWith (proofLtNBalance (Proxy::Proxy ('ForkTree l (Node n1 a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn)))) (Proxy::Proxy n)) Refl


-- | Prove that inserting a node with key 'x' (greater than 'n') and element value 'a'
-- | in a tree 't' which verifies 'GtN t n ~ 'True' preserves the GtN invariant,
-- | given that the comparison between 'x' and the root key of the tree equals 'o'.
-- | The 'o' parameter guides the proof.
class ProofGtNInsert' (x :: Nat) (a :: Type) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofGtNInsert' :: (CmpNat x n ~ 'GT, GtN t n ~ 'True) =>
    Node x a -> ITree t -> Proxy n -> Proxy o -> GtN (Insert x a t) n :~: 'True
instance (CmpNat x n1 ~ 'EQ) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) r) n 'EQ where
  proofGtNInsert' _ ForkITree{} _ _ = Refl
instance (CmpNat x n1 ~ 'LT,
  ProofGtNBalance ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n1 a1) r) n) =>
  ProofGtNInsert' x a ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofGtNInsert' _ (ForkITree EmptyITree _ _) _ _ =
    gcastWith (proofGtNBalance (Proxy::Proxy ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n1 a1) r)) (Proxy::Proxy n)) Refl
instance (CmpNat x n1 ~ 'LT, l ~ 'ForkTree ll (Node ln lna) lr, GtN l n ~ 'True,
  ProofGtNInsert' x a l n (CmpNat x ln), ProofGtNBalance ('ForkTree (Insert' x a ('ForkTree ll (Node ln lna) lr) (CmpNat x ln)) (Node n1 a1) r) n) =>
  ProofGtNInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n1 a1) r) n 'LT where
  proofGtNInsert' x (ForkITree l@ForkITree{} _ _) n _ =
    gcastWith (proofGtNInsert' x l n (Proxy::Proxy (CmpNat x ln))) $
      gcastWith (proofGtNBalance (Proxy::Proxy ('ForkTree (Insert' x a ('ForkTree ll (Node ln lna) lr) (CmpNat x ln)) (Node n1 a1) r)) (Proxy::Proxy n)) Refl
instance (CmpNat x n1 ~ 'GT, ProofGtNBalance ('ForkTree l (Node n1 a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree)) n) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofGtNInsert' _ (ForkITree _ _ EmptyITree) _ _ =
    gcastWith (proofGtNBalance (Proxy::Proxy ('ForkTree l (Node n1 a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree))) (Proxy::Proxy n)) Refl
instance (CmpNat x n1 ~ 'GT, r ~ 'ForkTree rl (Node rn rna) rr, GtN r n ~ 'True,
  ProofGtNInsert' x a r n (CmpNat x rn), ProofGtNBalance ('ForkTree l (Node n1 a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn))) n) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn rna) rr)) n 'GT where
  proofGtNInsert' x (ForkITree _ _ r@ForkITree{}) n _ =
    gcastWith (proofGtNInsert' x r n (Proxy::Proxy (CmpNat x rn))) $
      gcastWith (proofGtNBalance (Proxy::Proxy ('ForkTree l (Node n1 a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn)))) (Proxy::Proxy n)) Refl


-- | Prove that applying a rebalancing (a composition of rotations)
-- | to a BST tree preserves BST condition.
-- | The BST invariant was already check since this proof is called after proofs for Insert or Delete.
class ProofIsBSTBalance (t :: Tree) where
  proofIsBSTBalance :: (IsBST t ~ 'True) =>
    Proxy t -> IsBST (Balance t) :~: 'True
instance ProofIsBSTBalance 'EmptyTree where
  proofIsBSTBalance _ = Refl
instance (ProofIsBSTBalance' ('ForkTree l (Node n a) r) (UnbalancedState (Height l) (Height r))) =>
  ProofIsBSTBalance ('ForkTree l (Node n a) r) where
  proofIsBSTBalance pt = gcastWith (proofIsBSTBalance' pt (Proxy::Proxy (UnbalancedState (Height l) (Height r)))) Refl

-- | Prove that applying a rebalancing (a composition of rotations)
-- | to a BST tree preserves BST condition, given the comparison 'us' of the heights of the left and right sub trees.
-- | This is called only from ProofIsBSTBalance.
-- | The BST invariant was already check since this proof is called after proofs for Insert or Delete.
-- | The 'us' parameter guides the proof.
class ProofIsBSTBalance' (t :: Tree) (us :: US) where
  proofIsBSTBalance' :: (IsBST t ~ 'True) =>
    Proxy t -> Proxy us -> IsBST (Balance' t us) :~: 'True
instance (IsBST ('ForkTree l (Node n a) r) ~ 'True) =>
  ProofIsBSTBalance' ('ForkTree l (Node n a) r) 'NotUnbalanced where
  proofIsBSTBalance' _ _ = Refl
instance (ProofIsBSTRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced (BalancedState (Height ll) (Height lr))) =>
  ProofIsBSTBalance' ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced where
  proofIsBSTBalance' pt pus = gcastWith (proofIsBSTRotate pt pus (Proxy::Proxy (BalancedState (Height ll) (Height lr)))) Refl
instance ProofIsBSTRotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced (BalancedState (Height rl) (Height rr)) =>
  ProofIsBSTBalance' ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced where
  proofIsBSTBalance' pt pus = gcastWith (proofIsBSTRotate pt pus (Proxy::Proxy (BalancedState (Height rl) (Height rr)))) Refl


-- | Prove that applying a rotation
-- | to a BST tree preserves BST condition.
-- | The BST invariant was already check since this proof is called after proofs for Insert or Delete.
-- | Each instance needs some set of hypotesis. However, all these are deduced from the fact that 'IsBST ~ 'True'.
-- | However, the compiler is not able to infer this. Instead of requesting these hypotesis in the context of
-- | every instance (which would increase the computational effort), unsafeCoerce is used.
-- | The hypotesis that the compiler needs are commented within the code.
class ProofIsBSTRotate (t :: Tree) (us :: US) (bs :: BS) where
  proofIsBSTRotate :: (IsBST t ~ 'True) =>
    Proxy t -> Proxy us -> Proxy bs -> IsBST (Rotate t us bs) :~: 'True

-- | Left-Left case (Right rotation)
-- | IsBST ll ~ 'True, IsBST lr ~ 'True, IsBST r ~ 'True, LtN lr n ~ 'True, GtN r n ~ 'True, LtN ll ln ~ 'True, CmpNat n ln ~ 'GT,
-- | GtN lr ln ~ 'True, GtN r ln ~ 'True
instance ProofIsBSTRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'LeftHeavy where
  proofIsBSTRotate _ _ _ = unsafeCoerce Refl
-- | IsBST ll ~ 'True, IsBST lr ~ 'True, IsBST r ~ 'True, LtN lr n ~ 'True, GtN r n ~ 'True, LtN ll ln ~ 'True, CmpNat n ln ~ 'GT,
-- | GtN lr ln ~ 'True, GtN r ln ~ 'True
instance ProofIsBSTRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'Balanced where
  proofIsBSTRotate _ _ _ = unsafeCoerce Refl

-- | Right-Right case (Left rotation)
-- | IsBST l ~ 'True, IsBST rl ~ 'True, LtN l n ~ 'True, GtN rl n ~ 'True, IsBST rr ~ 'True, CmpNat n rn ~ 'LT, LtN l rn ~ 'True,
-- | LtN rl rn ~ 'True, GtN rr rn ~ 'True
instance ProofIsBSTRotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'RightHeavy where
  proofIsBSTRotate _ _ _ = unsafeCoerce Refl
-- | IsBST l ~ 'True, IsBST rl ~ 'True, LtN l n ~ 'True, GtN rl n ~ 'True, IsBST rr ~ 'True, CmpNat n rn ~ 'LT, LtN l rn ~ 'True,
-- | LtN rl rn ~ 'True, GtN rr rn ~ 'True
instance ProofIsBSTRotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'Balanced where
  proofIsBSTRotate _ _ _ = unsafeCoerce Refl

-- | Left-Right case (First left rotation, then right rotation)
-- | IsBST ll ~ 'True, IsBST lrl ~ 'True, LtN ll ln ~ 'True, GtN lrl ln ~ 'True, IsBST lrr ~ 'True, IsBST r ~ 'True, LtN lrr n ~ 'True,
-- | GtN r n ~ 'True, CmpNat ln lrn ~ 'LT, LtN ll lrn ~ 'True, LtN lrl lrn ~ 'True, CmpNat n lrn ~ 'GT, GtN lrr lrn ~ 'True, GtN r lrn ~ 'True
instance ProofIsBSTRotate ('ForkTree ('ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr)) (Node n a) r) 'LeftUnbalanced 'RightHeavy where
  proofIsBSTRotate _ _ _ = unsafeCoerce Refl

-- | Right-Left case (First right rotation, then left rotation)
-- | IsBST l ~ 'True, IsBST rll ~ 'True, LtN l n ~ 'True, GtN rll n ~ 'True, IsBST rlr ~ 'True, IsBST rr ~ 'True, LtN rlr rn ~ 'True,
-- | GtN rr rn ~ 'True, CmpNat n rln ~ 'LT, LtN l rln ~ 'True, LtN rll rln ~ 'True, CmpNat rn rln ~ 'GT, GtN rlr rln ~ 'True, GtN rr rln ~ 'True
instance ProofIsBSTRotate ('ForkTree l (Node n a) ('ForkTree ('ForkTree rll (Node rln rla) rlr) (Node rn ra) rr)) 'RightUnbalanced 'LeftHeavy where
  proofIsBSTRotate _ _ _ = unsafeCoerce Refl


-- | Prove that rebalancing a tree 't' which verifies 'LtN t n ~ 'True' preserves the LtN invariant.
class ProofLtNBalance (t :: Tree) (n :: Nat) where
  proofLtNBalance :: (LtN t n ~ 'True) =>
    Proxy t -> Proxy n -> LtN (Balance t) n :~: 'True
instance ProofLtNBalance 'EmptyTree n where
  proofLtNBalance _ _ = Refl
instance (ProofLtNBalance' ('ForkTree l (Node n1 a) r) n (UnbalancedState (Height l) (Height r))) =>
  ProofLtNBalance ('ForkTree l (Node n1 a) r) n where
  proofLtNBalance pt pn = gcastWith (proofLtNBalance' pt pn (Proxy::Proxy (UnbalancedState (Height l) (Height r)))) Refl

-- | Prove that rebalancing a tree 't' which verifies 'LtN t n ~ 'True' preserves the LtN invariant,
-- | given the unbalanced state 'us' of the tree.
-- | The 'us' parameter guides the proof.
class ProofLtNBalance' (t :: Tree) (n :: Nat) (us :: US) where
  proofLtNBalance' :: (LtN t n ~ 'True) =>
    Proxy t -> Proxy n -> Proxy us -> LtN (Balance' t us) n :~: 'True
instance ProofLtNBalance' ('ForkTree l (Node n1 a) r) n 'NotUnbalanced where
  proofLtNBalance' _ _ _ = Refl
instance (ProofLtNRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced (BalancedState (Height ll) (Height lr))) =>
  ProofLtNBalance' ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced where
  proofLtNBalance' pt pn pus = gcastWith (proofLtNRotate pt pn pus (Proxy::Proxy (BalancedState (Height ll) (Height lr)))) Refl
instance ProofLtNRotate ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced (BalancedState (Height rl) (Height rr)) =>
  ProofLtNBalance' ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced where
  proofLtNBalance' pt pn pus = gcastWith (proofLtNRotate pt pn pus (Proxy::Proxy (BalancedState (Height rl) (Height rr)))) Refl


-- | Prove that applying a rotation to a tree 't' which verifies 'LtN t n ~ 'True' preserves the LtN invariant.
-- | Each instance needs some set of hypotesis. However, all these are deduced from the fact that 'LtN t n ~ 'True'.
-- | However, the compiler is not able to infer this. Instead of requesting these hypotesis in the context of
-- | every instance (which would increase the computational effort), unsafeCoerce is used.
-- | The hypotesis that the compiler needs are commented within the code.
class ProofLtNRotate (t :: Tree) (n :: Nat) (us :: US) (bs :: BS) where
  proofLtNRotate :: (LtN t n ~ 'True) =>
    Proxy t -> Proxy n -> Proxy us -> Proxy bs -> LtN (Rotate t us bs) n :~: 'True

-- | Left-Left case (Right rotation)
-- | l ~ ('ForkTree ll (Node ln la) lr), LtN l n ~ 'True, CmpNat n1 n ~ 'LT, LtN r n ~ 'True
instance ProofLtNRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced 'LeftHeavy where
  proofLtNRotate _ _ _ _ = unsafeCoerce Refl
-- | l ~ ('ForkTree ll (Node ln la) lr), LtN l n ~ 'True, CmpNat n1 n ~ 'LT, LtN r n ~ 'True
instance ProofLtNRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced 'Balanced where
  proofLtNRotate _ _ _ _ = unsafeCoerce Refl

-- | Right-Right case (Left rotation)
-- | r ~ ('ForkTree rl (Node rn ra) rr), LtN l n ~ 'True, LtN r n ~ 'True, CmpNat n1 n ~ 'LT
instance ProofLtNRotate ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced 'RightHeavy where
  proofLtNRotate _ _ _ _ = unsafeCoerce Refl
-- | r ~ ('ForkTree rl (Node rn ra) rr), LtN l n ~ 'True, LtN r n ~ 'True, CmpNat n1 n ~ 'LT
instance ProofLtNRotate ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced 'Balanced where
  proofLtNRotate _ _ _ _ = unsafeCoerce Refl

-- | Left-Right case (First left rotation, then right rotation)
-- | LtN lrr n ~ 'True, CmpNat lrn n ~ 'LT, CmpNat ln n ~ 'LT, LtN ll n ~ 'True, LtN lrl n ~ 'True,
-- | CmpNat n1 n ~ 'LT, LtN r n ~ 'True
instance (LtN ll ln ~ 'True, CmpNat ln lrn ~ 'LT, LtN ll lrn ~ 'True, LtN lrl lrn ~ 'True) =>
  ProofLtNRotate ('ForkTree ('ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr)) (Node n1 a) r) n 'LeftUnbalanced 'RightHeavy where
  proofLtNRotate _ _ _ _ = unsafeCoerce Refl

-- | Right-Left case (First right rotation, then left rotation)
-- | LtN l n ~ 'True, CmpNat n rln ~ 'LT, CmpNat rln n ~ 'LT, CmpNat n1 n ~ 'LT, LtN rll n ~ 'True,
-- | CmpNat rn n ~ 'LT, LtN rr n ~ 'True, LtN rlr n ~ 'True
instance (LtN rlr rn ~ 'True, LtN l rln ~ 'True, LtN rll rln ~ 'True) =>
  ProofLtNRotate ('ForkTree l (Node n1 a) ('ForkTree ('ForkTree rll (Node rln rla) rlr) (Node rn ra) rr)) n 'RightUnbalanced 'LeftHeavy where
  proofLtNRotate _ _ _ _ = unsafeCoerce Refl


-- | Prove that rebalancing a tree 't' which verifies 'GtN t n ~ 'True' preserves the GtN invariant.
class ProofGtNBalance (t :: Tree) (n :: Nat) where
  proofGtNBalance :: (GtN t n ~ 'True) =>
    Proxy t -> Proxy n -> GtN (Balance t) n :~: 'True
instance ProofGtNBalance 'EmptyTree n where
  proofGtNBalance _ _ = Refl
instance (ProofGtNBalance' ('ForkTree l (Node n1 a) r) n (UnbalancedState (Height l) (Height r))) =>
  ProofGtNBalance ('ForkTree l (Node n1 a) r) n where
  proofGtNBalance pt pn = gcastWith (proofGtNBalance' pt pn (Proxy::Proxy (UnbalancedState (Height l) (Height r)))) Refl

-- | Prove that rebalancing a tree 't' which verifies 'GtN t n ~ 'True' preserves the GtN invariant,
-- | given the unbalanced state 'us' of the tree.
-- | The 'us' parameter guides the proof.
class ProofGtNBalance' (t :: Tree) (n :: Nat) (us :: US) where
  proofGtNBalance' :: (GtN t n ~ 'True) =>
    Proxy t -> Proxy n -> Proxy us -> GtN (Balance' t us) n :~: 'True
instance ProofGtNBalance' ('ForkTree l (Node n1 a) r) n 'NotUnbalanced where
  proofGtNBalance' _ _ _ = Refl
instance (ProofGtNRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced (BalancedState (Height ll) (Height lr))) =>
  ProofGtNBalance' ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced where
  proofGtNBalance' pt pn pus = gcastWith (proofGtNRotate pt pn pus (Proxy::Proxy (BalancedState (Height ll) (Height lr)))) Refl
instance ProofGtNRotate ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced (BalancedState (Height rl) (Height rr)) =>
  ProofGtNBalance' ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced where
  proofGtNBalance' pt pn pus = gcastWith (proofGtNRotate pt pn pus (Proxy::Proxy (BalancedState (Height rl) (Height rr)))) Refl


-- | Prove that applying a rotation to a tree 't' which verifies 'GtN t n ~ 'True' preserves the GtN invariant.
-- | Each instance needs some set of hypotesis. However, all these are deduced from the fact that 'GtN t n ~ 'True'.
-- | However, the compiler is not able to infer this. Instead of requesting these hypotesis in the context of
-- | every instance (which would increase the computational effort), unsafeCoerce is used.
-- | The hypotesis that the compiler needs are commented within the code.
class ProofGtNRotate (t :: Tree) (n :: Nat) (us::US) (bs::BS) where
  proofGtNRotate :: (GtN t n ~ 'True) =>
    Proxy t -> Proxy n -> Proxy us -> Proxy bs -> GtN (Rotate t us bs) n :~: 'True

-- | Left-Left case (Right rotation)
-- | GtN r n ~ 'True, CmpNat ln n ~ 'GT, GtN ll n ~ 'True, CmpNat n1 n ~ 'GT, GtN lr n ~ 'True
instance ProofGtNRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced 'LeftHeavy where
  proofGtNRotate _ _ _ _ = unsafeCoerce Refl
-- | CmpNat ln n ~ 'GT, GtN ll n ~ 'True, CmpNat n1 n ~ 'GT, GtN r n ~ 'True, GtN lr n ~ 'True
instance ProofGtNRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced 'Balanced where
  proofGtNRotate _ _ _ _ = unsafeCoerce Refl

-- | Right-Right case (Left rotation)
-- | CmpNat n1 n ~ 'GT, GtN rl n ~ 'True, GtN rr n ~ 'True, GtN l n ~ 'True
instance ProofGtNRotate ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced 'RightHeavy where
  proofGtNRotate _ _ _ _ = unsafeCoerce Refl
-- | CmpNat n1 n ~ 'GT, GtN rl n ~ 'True, GtN rr n ~ 'True, GtN l n ~ 'True
instance ProofGtNRotate ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced 'Balanced where
  proofGtNRotate _ _ _ _ = unsafeCoerce Refl

-- | Left-Right case (First left rotation, then right rotation)
-- | CmpNat ln n ~ 'GT, GtN ll n ~ 'True, CmpNat n1 n ~ 'GT, GtN r n ~ 'True
instance ProofGtNRotate ('ForkTree ('ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr)) (Node n1 a) r) n 'LeftUnbalanced 'RightHeavy where
  proofGtNRotate _ _ _ _ = unsafeCoerce Refl

-- | Right-Left case (First right rotation, then left rotation)
-- | CmpNat n1 n ~ 'GT, CmpNat rn n ~ 'GT, GtN rr n ~ 'True, GtN l n ~ 'True
instance ProofGtNRotate ('ForkTree l (Node n1 a) ('ForkTree ('ForkTree rll (Node rln rla) rlr) (Node rn ra) rr)) n 'RightUnbalanced 'LeftHeavy where
  proofGtNRotate _ _ _ _ = unsafeCoerce Refl


class ProofIsAVLBalance' (t :: Tree) (us::US) where
  proofIsAVLBalance' :: Proxy t -> Proxy us -> IsAVL (Balance' t us) :~: 'True
instance (IsAVL ('ForkTree l (Node n a) r) ~ 'True) =>
  ProofIsAVLBalance' ('ForkTree l (Node n a) r) 'NotUnbalanced where
  proofIsAVLBalance' _ _ = Refl
instance (ProofIsAVLRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced (BalancedState (Height ll) (Height lr))) =>
  ProofIsAVLBalance' ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced where
  proofIsAVLBalance' pt pus = gcastWith (proofIsAVLRotate pt pus (Proxy::Proxy (BalancedState (Height ll) (Height lr)))) Refl
instance ProofIsAVLRotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced (BalancedState (Height rl) (Height rr)) =>
  ProofIsAVLBalance' ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced where
  proofIsAVLBalance' pt pus = gcastWith (proofIsAVLRotate pt pus (Proxy::Proxy (BalancedState (Height rl) (Height rr)))) Refl

class ProofIsAVLRotate (t :: Tree) (us::US) (bs::BS) where
  proofIsAVLRotate :: Proxy t -> Proxy us -> Proxy bs -> IsAVL (Rotate t us bs) :~: 'True
-- | Left-Left case (Right rotation)
instance (IsAVL ll ~ 'True, IsAVL lr ~ 'True, IsAVL r ~ 'True,
  BalancedHeights (Height lr) (Height r) ~ 'True, BalancedHeights (Height ll) (1 + If (Height lr <=? Height r) (Height r) (Height lr)) ~ 'True) =>
  ProofIsAVLRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'LeftHeavy where
  proofIsAVLRotate _ _ _ = Refl
instance (IsAVL ll ~ 'True, IsAVL lr ~ 'True, IsAVL r ~ 'True,
  BalancedHeights (Height ll) (1 + If (Height lr <=? Height r) (Height r) (Height lr)) ~ 'True, BalancedHeights (Height lr) (Height r) ~ 'True) =>
  ProofIsAVLRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'Balanced where
  proofIsAVLRotate _ _ _ = Refl
-- | Right-Right case (Left rotation)
instance (IsAVL l ~ 'True, IsAVL rl ~ 'True, IsAVL rr ~ 'True,
  BalancedHeights (1 + If (Height l <=? Height rl) (Height rl) (Height l)) (Height rr) ~ 'True, BalancedHeights (Height l) (Height rl) ~ 'True) =>
  ProofIsAVLRotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'RightHeavy where
  proofIsAVLRotate _ _ _ = Refl
instance (IsAVL l ~ 'True, IsAVL rl ~ 'True, IsAVL rr ~ 'True,
  BalancedHeights (1 + If (Height l <=? Height rl) (Height rl) (Height l)) (Height rr) ~ 'True, BalancedHeights (Height l) (Height rl) ~ 'True) =>
  ProofIsAVLRotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'Balanced where
  proofIsAVLRotate _ _ _ = Refl
-- | Left-Right case (First left rotation, then right rotation)
instance (IsAVL ll ~ 'True, IsAVL lrl ~ 'True, IsAVL lrr ~ 'True, IsAVL r ~ 'True,
  BalancedHeights (1 + If (Height ll <=? Height lrl) (Height lrl) (Height ll)) (1 + If (Height lrr <=? Height r) (Height r) (Height lrr)) ~ 'True,
  BalancedHeights (Height ll) (Height lrl) ~ 'True, BalancedHeights (Height lrr) (Height r) ~ 'True) =>
  ProofIsAVLRotate ('ForkTree ('ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr)) (Node n a) r) 'LeftUnbalanced 'RightHeavy where
  proofIsAVLRotate _ _ _ = Refl
-- | Right-Left case (First right rotation, then left rotation)
instance (IsAVL l ~ 'True, IsAVL rll ~ 'True, IsAVL rlr ~ 'True, IsAVL rr ~ 'True,
  BalancedHeights (1 + If (Height l <=? Height rll) (Height rll) (Height l)) (1 + If (Height rlr <=? Height rr) (Height rr) (Height rlr)) ~ 'True,
  BalancedHeights (Height l) (Height rll) ~ 'True, BalancedHeights (Height rlr) (Height rr) ~ 'True) =>
  ProofIsAVLRotate ('ForkTree l (Node n a) ('ForkTree ('ForkTree rll (Node rln rla) rlr) (Node rn ra) rr)) 'RightUnbalanced 'LeftHeavy where
  proofIsAVLRotate _ _ _ = Refl

class ProofIsAVLInsert (x :: Nat) (a :: Type) (t :: Tree) where
  proofIsAVLInsert :: (IsAVL t ~ 'True) =>
    Node x a -> ITree t -> IsAVL (Insert x a t) :~: 'True
instance ProofIsAVLInsert x a 'EmptyTree where
  proofIsAVLInsert _ EmptyITree = Refl
instance ProofIsAVLInsert' x a ('ForkTree l (Node n a1) r) (CmpNat x n) => ProofIsAVLInsert x a ('ForkTree l (Node n a1) r) where
  proofIsAVLInsert node t = proofIsAVLInsert' node t (Proxy::Proxy (CmpNat x n))

class ProofIsAVLInsert' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) where
  proofIsAVLInsert' :: (t ~ 'ForkTree l (Node n a1) r) => Node x a -> ITree t -> Proxy o -> IsAVL (Insert' x a t o) :~: 'True
instance (IsAVL l ~ 'True, IsAVL r ~ 'True, BalancedHeights (Height l) (Height r) ~ ' True) =>
  ProofIsAVLInsert' x a ('ForkTree l (Node n a1) r) 'EQ where
  proofIsAVLInsert' _ ForkITree{} _ = Refl
instance (l ~ 'EmptyTree, ProofIsAVLBalance' ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n a1) r) (UnbalancedState 1 (Height r))) =>
  ProofIsAVLInsert' x a ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  proofIsAVLInsert' _ (ForkITree EmptyITree _ _) _ =  gcastWith (proofIsAVLBalance' (Proxy::Proxy ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n a1) r)) (Proxy::Proxy (UnbalancedState 1 (Height r)))) Refl
instance (l ~ 'ForkTree ll (Node ln lna) lr, ProofIsAVLInsert' x a l (CmpNat x ln),
  ProofIsAVLBalance' ('ForkTree (Insert' x a l (CmpNat x ln)) (Node n a1) r) (UnbalancedState (Height (Insert' x a ('ForkTree ll (Node ln lna) lr) (CmpNat x ln))) (Height r))) =>
  ProofIsAVLInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT where
  proofIsAVLInsert' node (ForkITree l@ForkITree{} _ _) _ =
    gcastWith (proofIsAVLInsert' node l (Proxy::Proxy (CmpNat x ln))) $
      gcastWith (proofIsAVLBalance' (Proxy::Proxy ('ForkTree (Insert' x a l (CmpNat x ln)) (Node n a1) r)) (Proxy::Proxy (UnbalancedState (Height (Insert' x a ('ForkTree ll (Node ln lna) lr) (CmpNat x ln))) (Height r)))) Refl
instance (r ~ 'EmptyTree, ProofIsAVLBalance' ('ForkTree l (Node n a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree)) (UnbalancedState (Height l) 1)) =>
  ProofIsAVLInsert' x a ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  proofIsAVLInsert' _ (ForkITree _ _ EmptyITree) _ = gcastWith (proofIsAVLBalance' (Proxy::Proxy ('ForkTree l (Node n a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree))) (Proxy::Proxy (UnbalancedState (Height l) 1))) Refl
instance (r ~ 'ForkTree rl (Node rn rna) rr, ProofIsAVLInsert' x a r (CmpNat x rn),
  ProofIsAVLBalance' ('ForkTree l (Node n a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn))) (UnbalancedState (Height l) (Height (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn))))) =>
  ProofIsAVLInsert' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT where
  proofIsAVLInsert' node (ForkITree _ _ r@ForkITree{}) _ =
    gcastWith (proofIsAVLInsert' node r (Proxy::Proxy (CmpNat x rn))) $
      gcastWith (proofIsAVLBalance' (Proxy::Proxy ('ForkTree l (Node n a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn)))) (Proxy::Proxy (UnbalancedState (Height l) (Height (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn)))))) Refl

class ProofIsBSTDelete (x :: Nat) (t :: Tree) where
  proofIsBSTDelete :: (IsBST t ~ 'True) =>
    Proxy x -> ITree t -> IsBST (Delete x t) :~: 'True
instance ProofIsBSTDelete x 'EmptyTree where
  proofIsBSTDelete _ EmptyITree = Refl
instance ProofIsBSTDelete' x ('ForkTree l (Node n a1) r) (CmpNat x n) =>
  ProofIsBSTDelete x ('ForkTree l (Node n a1) r) where
  proofIsBSTDelete px t@ForkITree{} = proofIsBSTDelete' px t (Proxy::Proxy (CmpNat x n))

class ProofIsBSTDelete' (x :: Nat) (t :: Tree) (o :: Ordering) where
  proofIsBSTDelete' :: (t ~ 'ForkTree l (Node n a1) r) =>
    Proxy x -> ITree t -> Proxy o -> IsBST (Delete' x t o) :~: 'True
instance ProofIsBSTDelete' x ('ForkTree 'EmptyTree (Node n a1) 'EmptyTree) 'EQ where
  proofIsBSTDelete' _ (ForkITree EmptyITree (Node _) EmptyITree) _ = Refl
instance (IsBST rl ~ 'True, IsBST rr ~ 'True, LtN rl rn ~ 'True, GtN rr rn ~ 'True) =>
  ProofIsBSTDelete' x ('ForkTree 'EmptyTree (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  proofIsBSTDelete' _ (ForkITree EmptyITree (Node _) ForkITree{}) _ = Refl
instance (IsBST ll ~ 'True, IsBST lr ~ 'True, LtN ll ln ~ 'True, GtN lr ln ~ 'True) =>
  ProofIsBSTDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) 'EmptyTree) 'EQ where
  proofIsBSTDelete' _ (ForkITree ForkITree{} (Node _) EmptyITree) _ = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, IsBST l ~ 'True, MaxKeyDeletable l, ProofMaxKeyDeleteIsBST l,
  r ~ 'ForkTree rl (Node rn ra) rr, IsBST r ~ 'True,
  ProofLtNMaxKeyDeleteMaxKey l, Maxable l,
  t ~ 'ForkTree l (Node n a1) r, IsBST t ~ 'True, ProofGtNMaxKey t,
  ProofIsBSTBalance ('ForkTree (MaxKeyDelete l) (Node (MaxKey l) (MaxValue l)) r)) =>
  ProofIsBSTDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  proofIsBSTDelete' _ t@(ForkITree l@ForkITree{} (Node _) ForkITree{}) _ =
    gcastWith (proofGtNMaxKey t) $
      gcastWith (proofLtNMaxKeyDeleteMaxKey l) $
        gcastWith (proofMaxKeyDeleteIsBST l) $
          gcastWith (proofIsBSTBalance (Proxy::Proxy ('ForkTree (MaxKeyDelete l) (Node (MaxKey l) (MaxValue l)) r))) Refl
instance (IsBST r ~ 'True, GtN r n ~ 'True) =>
  ProofIsBSTDelete' x ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  proofIsBSTDelete' _ (ForkITree EmptyITree (Node _) _) _ = Refl
instance (IsBST r ~ 'True, GtN r n ~ 'True, LtN ('ForkTree ll (Node ln la) lr) n ~ 'True, ProofIsBSTDelete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln),
  CmpNat x n ~ 'LT, ProofLtNDelete' x ('ForkTree ll (Node ln la) lr) n (CmpNat x ln),
  ProofIsBSTBalance ('ForkTree (Delete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln)) (Node n a1) r)) =>
  ProofIsBSTDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) r) 'LT where
  proofIsBSTDelete' px (ForkITree l@ForkITree{} _ _) _ =
    gcastWith (proofLtNDelete' px l (Proxy::Proxy n) (Proxy::Proxy (CmpNat x ln))) $
      gcastWith (proofIsBSTDelete' px l (Proxy::Proxy (CmpNat x ln))) $
        gcastWith (proofIsBSTBalance (Proxy::Proxy ('ForkTree (Delete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln)) (Node n a1) r))) Refl
instance (IsBST l ~ 'True, LtN l n ~ 'True) =>
  ProofIsBSTDelete' x ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  proofIsBSTDelete' _ (ForkITree _ (Node _) EmptyITree) _ = Refl
instance (IsBST l ~ 'True, LtN l n ~ 'True, ProofIsBSTDelete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn),
  CmpNat x n ~ 'GT, GtN ('ForkTree rl (Node rn ra) rr) n ~ 'True, ProofGtNDelete' x ('ForkTree rl (Node rn ra) rr) n (CmpNat x rn),
  ProofIsBSTBalance ('ForkTree l (Node n a1) (Delete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn)))) =>
  ProofIsBSTDelete' x ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'GT where
  proofIsBSTDelete' px (ForkITree _ (Node _) r@ForkITree{}) _ =
    gcastWith (proofGtNDelete' px r (Proxy::Proxy n) (Proxy::Proxy (CmpNat x rn))) $
      gcastWith (proofIsBSTDelete' px r (Proxy::Proxy (CmpNat x rn))) $
        gcastWith (proofIsBSTBalance (Proxy::Proxy ('ForkTree l (Node n a1) (Delete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn))))) Refl

class ProofLtNDelete' (x :: Nat) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofLtNDelete' :: (t ~ 'ForkTree l n1 r, CmpNat x n ~ 'LT, LtN t n ~ 'True) =>
    Proxy x -> ITree t -> Proxy n -> Proxy o -> LtN (Delete' x t o) n :~: 'True
instance ProofLtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) 'EmptyTree) n 'EQ where
  proofLtNDelete' _ (ForkITree EmptyITree (Node _) EmptyITree) _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, IsBST r ~ 'True, LtN r n ~ 'True) =>
  ProofLtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ where
  proofLtNDelete' _ (ForkITree EmptyITree (Node _) ForkITree{}) _ _ = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, IsBST l ~ 'True, LtN l n ~ 'True) =>
  ProofLtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) 'EmptyTree) n 'EQ where
  proofLtNDelete' _ (ForkITree ForkITree{} (Node _) EmptyITree) _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, IsBST r ~ 'True, LtN r n ~ 'True,
  l ~ 'ForkTree ll (Node ln la) lr, LtN l n ~ 'True, ProofLTMaxKey l n, Maxable l,
  ProofLtNMaxKeyDelete l n, MaxKeyDeletable l,
  ProofLtNBalance ('ForkTree (MaxKeyDelete l) (Node (MaxKey l) (MaxValue l)) r) n) =>
  ProofLtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ where
  proofLtNDelete' _ (ForkITree l@ForkITree{} (Node _) ForkITree{}) _ _ =
    gcastWith (proofLtNMaxKeyDelete l (Proxy::Proxy n)) $
      gcastWith (proofLTMaxKey l (Proxy::Proxy n)) $
        gcastWith (proofLtNBalance (Proxy::Proxy ('ForkTree (MaxKeyDelete l) (Node (MaxKey l) (MaxValue l)) r)) (Proxy::Proxy n)) Refl
instance (IsBST r ~ 'True, LtN r n ~ 'True) =>
  ProofLtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofLtNDelete' _ (ForkITree EmptyITree (Node _) _) _ _ = Refl
instance (IsBST r ~ 'True, LtN r n ~ 'True, LtN ('ForkTree ll (Node ln la) lr) n ~ 'True, CmpNat n1 n ~ 'LT,
  ProofLtNDelete' x ('ForkTree ll (Node ln la) lr) n (CmpNat x ln),
  ProofLtNBalance ('ForkTree (Delete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln)) (Node n1 a1) r) n) =>
  ProofLtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) r) n 'LT where
  proofLtNDelete' px (ForkITree l@ForkITree{} _ _) _ _ =
    gcastWith (proofLtNDelete' px l (Proxy::Proxy n) (Proxy::Proxy (CmpNat x ln))) $
      gcastWith (proofLtNBalance (Proxy::Proxy ('ForkTree (Delete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln)) (Node n1 a1) r)) (Proxy::Proxy n)) Refl
instance (IsBST l ~ 'True, LtN l n ~ 'True) =>
  ProofLtNDelete' x ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofLtNDelete' _ (ForkITree _ (Node _) EmptyITree) _ _ = Refl
instance (IsBST l ~ 'True, LtN l n ~ 'True, CmpNat x n1 ~ 'GT, CmpNat n1 n ~ 'LT, LtN ('ForkTree rl (Node rn ra) rr) n ~ 'True,
  ProofLtNDelete' x ('ForkTree rl (Node rn ra) rr) n (CmpNat x rn),
  ProofLtNBalance ('ForkTree l (Node n1 a1) (Delete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn))) n) =>
  ProofLtNDelete' x ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'GT where
  proofLtNDelete' px (ForkITree _ (Node _) r@ForkITree{}) _ _ =
    gcastWith (proofLtNDelete' px r (Proxy::Proxy n) (Proxy::Proxy (CmpNat x rn))) $
      gcastWith (proofLtNBalance (Proxy::Proxy ('ForkTree l (Node n1 a1) (Delete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn)))) (Proxy::Proxy n)) Refl

class ProofGtNDelete' (x :: Nat) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofGtNDelete' :: (t ~ 'ForkTree l n1 r, CmpNat x n ~ 'GT, GtN t n ~ 'True) =>
    Proxy x -> ITree t -> Proxy n -> Proxy o -> GtN (Delete' x t o) n :~: 'True
instance ProofGtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) 'EmptyTree) n 'EQ where
  proofGtNDelete' _ (ForkITree EmptyITree (Node _) EmptyITree) _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, IsBST r ~ 'True, GtN r n ~ 'True) =>
  ProofGtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ where
  proofGtNDelete' _ (ForkITree EmptyITree (Node _) ForkITree{}) _ _ = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, IsBST l ~ 'True, GtN l n ~ 'True) =>
  ProofGtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) 'EmptyTree) n 'EQ where
  proofGtNDelete' _ (ForkITree ForkITree{} (Node _) EmptyITree) _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, IsBST r ~ 'True, GtN r n ~ 'True,
  l ~ 'ForkTree ll (Node ln la) lr, CmpNat ln n ~ 'GT, GtN l n ~ 'True, t ~ 'ForkTree l (Node n1 a1) r,
  ProofGTMaxKey l n, Maxable l, ProofGtNMaxKeyDelete l n, MaxKeyDeletable l,
  ProofGtNBalance ('ForkTree (MaxKeyDelete l) (Node (MaxKey l) (MaxValue l)) r) n) =>
  ProofGtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ where
  proofGtNDelete' _ (ForkITree l@ForkITree{} (Node _) ForkITree{}) _ _ =
    gcastWith (proofGtNMaxKeyDelete l (Proxy::Proxy n)) $
      gcastWith (proofGTMaxKey l (Proxy::Proxy n)) $
        gcastWith (proofGtNBalance (Proxy::Proxy ('ForkTree (MaxKeyDelete l) (Node (MaxKey l) (MaxValue l)) r)) (Proxy::Proxy n)) Refl
instance (IsBST r ~ 'True, GtN r n ~ 'True) =>
  ProofGtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofGtNDelete' _ (ForkITree EmptyITree (Node _) _) _ _ = Refl
instance (IsBST r ~ 'True, GtN r n ~ 'True, GtN ('ForkTree ll (Node ln la) lr) n ~ 'True, CmpNat n1 n ~ 'GT,
  ProofGtNDelete' x ('ForkTree ll (Node ln la) lr) n (CmpNat x ln),
  ProofGtNBalance ('ForkTree (Delete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln)) (Node n1 a1) r) n) =>
  ProofGtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) r) n 'LT where
  proofGtNDelete' px (ForkITree l@ForkITree{} _ _) _ _ =
    gcastWith (proofGtNDelete' px l (Proxy::Proxy n) (Proxy::Proxy (CmpNat x ln))) $
      gcastWith (proofGtNBalance (Proxy::Proxy ('ForkTree (Delete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln)) (Node n1 a1) r)) (Proxy::Proxy n)) Refl
instance (IsBST l ~ 'True, GtN l n ~ 'True) =>
  ProofGtNDelete' x ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofGtNDelete' _ (ForkITree _ (Node _) EmptyITree) _ _ = Refl
instance (IsBST l ~ 'True, GtN l n ~ 'True, CmpNat x n1 ~ 'GT, CmpNat n1 n ~ 'GT, GtN ('ForkTree rl (Node rn ra) rr) n ~ 'True,
  ProofGtNDelete' x ('ForkTree rl (Node rn ra) rr) n (CmpNat x rn),
  ProofGtNBalance ('ForkTree l (Node n1 a1) (Delete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn))) n) =>
  ProofGtNDelete' x ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'GT where
  proofGtNDelete' px (ForkITree _ (Node _) r@ForkITree{}) _ _ =
    gcastWith (proofGtNDelete' px r (Proxy::Proxy n) (Proxy::Proxy (CmpNat x rn))) $
      gcastWith (proofGtNBalance (Proxy::Proxy ('ForkTree l (Node n1 a1) (Delete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn)))) (Proxy::Proxy n)) Refl

class ProofMaxKeyDeleteIsBST (t :: Tree) where
  proofMaxKeyDeleteIsBST :: (IsBST t ~ 'True, t ~ 'ForkTree l (Node n a) r, MaxKeyDeletable t) =>
    ITree t -> IsBST (MaxKeyDelete t) :~: 'True
instance ProofMaxKeyDeleteIsBST ('ForkTree 'EmptyTree (Node n a) 'EmptyTree) where
  proofMaxKeyDeleteIsBST (ForkITree EmptyITree (Node _) EmptyITree) = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, IsBST l ~ 'True) =>
  ProofMaxKeyDeleteIsBST ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) 'EmptyTree) where
  proofMaxKeyDeleteIsBST (ForkITree ForkITree{} (Node _) EmptyITree) = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, IsBST l ~ 'True, r ~ 'ForkTree rl (Node rn ra) rr, t ~ 'ForkTree l (Node n a) r, IsBST r ~ 'True,
  LtN l n ~ 'True, ProofMaxKeyDeleteIsBST r, MaxKeyDeletable r,
  ProofGtNMaxKeyDelete r n) =>
  ProofMaxKeyDeleteIsBST ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) ('ForkTree rl (Node rn ra) rr)) where
  proofMaxKeyDeleteIsBST (ForkITree ForkITree{} (Node _) r@ForkITree{}) = gcastWith (proofGtNMaxKeyDelete r (Proxy::Proxy n)) (gcastWith (proofMaxKeyDeleteIsBST r) Refl)

class ProofLtNMaxKeyDeleteMaxKey (t :: Tree) where
  proofLtNMaxKeyDeleteMaxKey :: (IsBST t ~ 'True, MaxKeyDeletable t, Maxable t) =>
    ITree t -> LtN (MaxKeyDelete t) (MaxKey t) :~: 'True
instance ProofLtNMaxKeyDeleteMaxKey ('ForkTree 'EmptyTree (Node n a) 'EmptyTree) where
  proofLtNMaxKeyDeleteMaxKey (ForkITree EmptyITree (Node _) EmptyITree) = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, LtN l n ~ 'True) =>
  ProofLtNMaxKeyDeleteMaxKey ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) 'EmptyTree) where
  proofLtNMaxKeyDeleteMaxKey (ForkITree ForkITree{} (Node _) EmptyITree) = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, LtN l n ~ 'True, r ~ 'ForkTree rl (Node rn ra) rr, IsBST r ~ 'True,
  Maxable r, MaxKeyDeletable r, ProofLtNMaxKeyDeleteMaxKey r,
  t ~ 'ForkTree l (Node n a) r,
  GtN r n ~ 'True, CmpNat ln (MaxKey r) ~ 'LT, LtN ll (MaxKey r) ~ 'True, LtN lr (MaxKey r) ~ 'True,
  CmpNat n (MaxKey r) ~ 'LT) =>
  ProofLtNMaxKeyDeleteMaxKey ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) ('ForkTree rl (Node rn ra) rr)) where
  proofLtNMaxKeyDeleteMaxKey (ForkITree ForkITree{} (Node _) r@ForkITree{}) =
    gcastWith (proofLtNMaxKeyDeleteMaxKey r) Refl

class ProofGtNMaxKey (t :: Tree) where
  proofGtNMaxKey :: (t ~ 'ForkTree l (Node n a) r, IsBST t ~ 'True, Maxable l) =>
    ITree t -> GtN r (MaxKey l) :~: 'True
instance ProofGtNMaxKey ('ForkTree l (Node n a) 'EmptyTree) where
  proofGtNMaxKey (ForkITree _ (Node _) EmptyITree) = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, GtN r n ~ 'True, CmpNat rn (MaxKey l) ~ 'GT, GtN rl (MaxKey l) ~ 'True, GtN rr (MaxKey l) ~ 'True) =>
  ProofGtNMaxKey ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) where
  proofGtNMaxKey (ForkITree _ (Node _) ForkITree{}) = Refl

class ProofGTMaxKey (t :: Tree) (n :: Nat) where
  proofGTMaxKey :: (Maxable t, GtN t n ~ 'True) =>
    ITree t -> Proxy n -> CmpNat (MaxKey t) n :~: 'GT
instance (CmpNat n1 n ~ 'GT) =>
  ProofGTMaxKey ('ForkTree l (Node n1 a) 'EmptyTree) n where
  proofGTMaxKey (ForkITree _ (Node _) EmptyITree) _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, GtN r n ~ 'True, Maxable r, ProofGTMaxKey r n) =>
  ProofGTMaxKey ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofGTMaxKey (ForkITree _ (Node _) r@ForkITree{}) pn = gcastWith (proofGTMaxKey r pn) Refl

class ProofGtNMaxKeyDelete (t :: Tree) (n :: Nat) where
  proofGtNMaxKeyDelete :: (MaxKeyDeletable t, GtN t n ~ 'True) =>
    ITree t -> Proxy n -> GtN (MaxKeyDelete t) n :~: 'True
instance (t ~ 'ForkTree l (Node n1 a) 'EmptyTree, GtN t n ~ 'True, GtN l n ~ 'True) =>
  ProofGtNMaxKeyDelete ('ForkTree l (Node n1 a) 'EmptyTree) n where
  proofGtNMaxKeyDelete (ForkITree _ (Node _) EmptyITree) _ = Refl
instance (t ~ 'ForkTree l (Node n1 a) 'EmptyTree, GtN t n ~ 'True, GtN l n ~ 'True,
  r ~ 'ForkTree rl (Node rn ra) rr, ProofGtNMaxKeyDelete r n, MaxKeyDeletable r) =>
  ProofGtNMaxKeyDelete ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofGtNMaxKeyDelete (ForkITree _ (Node _) r@ForkITree{}) pn = gcastWith (proofGtNMaxKeyDelete r pn) Refl

class ProofLTMaxKey (t :: Tree) (n :: Nat) where
  proofLTMaxKey :: (Maxable t, LtN t n ~ 'True) =>
    ITree t -> Proxy n -> CmpNat (MaxKey t) n :~: 'LT
instance (CmpNat n1 n ~ 'LT) =>
  ProofLTMaxKey ('ForkTree l (Node n1 a) 'EmptyTree) n where
  proofLTMaxKey (ForkITree _ (Node _) EmptyITree) _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, LtN r n ~ 'True, Maxable r, ProofLTMaxKey r n) =>
  ProofLTMaxKey ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofLTMaxKey (ForkITree _ (Node _) r@ForkITree{}) pn = gcastWith (proofLTMaxKey r pn) Refl

class ProofLtNMaxKeyDelete (t :: Tree) (n :: Nat) where
  proofLtNMaxKeyDelete :: (MaxKeyDeletable t, LtN t n ~ 'True) =>
    ITree t -> Proxy n -> LtN (MaxKeyDelete t) n :~: 'True
instance (t ~ 'ForkTree l (Node n1 a) 'EmptyTree, LtN t n ~ 'True, LtN l n ~ 'True) =>
  ProofLtNMaxKeyDelete ('ForkTree l (Node n1 a) 'EmptyTree) n where
  proofLtNMaxKeyDelete (ForkITree _ (Node _) EmptyITree) _ = Refl
instance (t ~ 'ForkTree l (Node n1 a) 'EmptyTree, LtN t n ~ 'True, LtN l n ~ 'True,
  r ~ 'ForkTree rl (Node rn ra) rr, ProofLtNMaxKeyDelete r n, MaxKeyDeletable r) =>
  ProofLtNMaxKeyDelete ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofLtNMaxKeyDelete (ForkITree _ (Node _) r@ForkITree{}) pn = gcastWith (proofLtNMaxKeyDelete r pn) Refl

class ProofIsAVLDelete (x :: Nat) (t :: Tree) where
  proofIsAVLDelete :: (IsAVL t ~ 'True) =>
    Proxy x -> ITree t -> IsAVL (Delete x t) :~: 'True
instance ProofIsAVLDelete x 'EmptyTree where
  proofIsAVLDelete _ EmptyITree = Refl
instance ProofIsAVLDelete' x ('ForkTree l (Node n a1) r) (CmpNat x n) => ProofIsAVLDelete x ('ForkTree l (Node n a1) r) where
  proofIsAVLDelete px t = proofIsAVLDelete' px t (Proxy::Proxy (CmpNat x n))

class ProofIsAVLDelete' (x :: Nat) (t :: Tree) (o :: Ordering) where
  proofIsAVLDelete' :: (t ~ 'ForkTree l (Node n a1) r) => Proxy x -> ITree t -> Proxy o -> IsAVL (Delete' x t o) :~: 'True
instance ProofIsAVLDelete' x ('ForkTree 'EmptyTree (Node n a1) 'EmptyTree) 'EQ where
  proofIsAVLDelete' _ (ForkITree EmptyITree (Node _) EmptyITree) _ = Refl
instance (BalancedHeights (Height rl) (Height rr) ~ 'True, IsAVL rl ~ 'True, IsAVL rr ~ 'True) =>
  ProofIsAVLDelete' x ('ForkTree 'EmptyTree (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  proofIsAVLDelete' _ (ForkITree EmptyITree (Node _) ForkITree{}) _ = Refl
instance (BalancedHeights (Height ll) (Height lr) ~ 'True, IsAVL ll ~ 'True, IsAVL lr ~ 'True) =>
  ProofIsAVLDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) 'EmptyTree) 'EQ where
  proofIsAVLDelete' _ (ForkITree ForkITree{} (Node _) EmptyITree) _ = Refl
instance (ProofIsAVLBalance' ('ForkTree (MaxKeyDelete ('ForkTree ll (Node ln la) lr)) (Node (MaxKey ('ForkTree ll (Node ln la) lr)) (MaxValue ('ForkTree ll (Node ln la) lr))) ('ForkTree rl (Node rn ra) rr)) (UnbalancedState (Height (MaxKeyDelete ('ForkTree ll (Node ln la) lr))) (1 + If (Height rl <=? Height rr) (Height rr) (Height rl)))) =>
  ProofIsAVLDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  proofIsAVLDelete' _ (ForkITree ForkITree{} (Node _) ForkITree{}) _ =
    gcastWith (proofIsAVLBalance' (Proxy::Proxy ('ForkTree (MaxKeyDelete ('ForkTree ll (Node ln la) lr)) (Node (MaxKey ('ForkTree ll (Node ln la) lr)) (MaxValue ('ForkTree ll (Node ln la) lr))) ('ForkTree rl (Node rn ra) rr))) (Proxy::Proxy (UnbalancedState (Height (MaxKeyDelete ('ForkTree ll (Node ln la) lr))) (1 + If (Height rl <=? Height rr) (Height rr) (Height rl))))) Refl
instance (IsAVL ('ForkTree 'EmptyTree (Node n a1) r) ~ 'True) =>
  ProofIsAVLDelete' x ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  proofIsAVLDelete' _ (ForkITree EmptyITree (Node _) _) _ = Refl
instance (ProofIsAVLBalance' ('ForkTree (Delete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln)) (Node n a1) r) (UnbalancedState (Height (Delete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln))) (Height r))) =>
  ProofIsAVLDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) r) 'LT where
  proofIsAVLDelete' _ (ForkITree ForkITree{} _ _) _ =
    gcastWith (proofIsAVLBalance' (Proxy::Proxy ('ForkTree (Delete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln)) (Node n a1) r)) (Proxy::Proxy (UnbalancedState (Height (Delete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln))) (Height r)))) Refl
instance (IsAVL ('ForkTree l (Node n a1) 'EmptyTree) ~ 'True) =>
  ProofIsAVLDelete' x ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  proofIsAVLDelete' _ (ForkITree _ (Node _) EmptyITree) _ = Refl
instance (ProofIsAVLBalance' ('ForkTree l (Node n a1) (Delete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn))) (UnbalancedState (Height l) (Height (Delete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn))))) =>
  ProofIsAVLDelete' x ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'GT where
  proofIsAVLDelete' _ (ForkITree _ _ ForkITree{}) _ =
    gcastWith (proofIsAVLBalance' (Proxy::Proxy ('ForkTree l (Node n a1) (Delete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn)))) (Proxy::Proxy (UnbalancedState (Height l) (Height (Delete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn)))))) Refl
