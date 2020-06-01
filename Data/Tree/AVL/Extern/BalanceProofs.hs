{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Tree.AVL.Extern.BalanceProofs (
  ProofIsBSTBalance(proofIsBSTBalance),
  ProofLtNBalance(proofLtNBalance),
  ProofGtNBalance(proofGtNBalance),
  ProofIsAVLBalance(proofIsAVLBalance)
) where

import           Data.Proxy                   (Proxy (Proxy))
import           Data.Tree.AVL.Extern.Balance (Balanceable (Balance),
                                               Balanceable' (Balance'),
                                               Rotateable (Rotate))
import           Data.Tree.AVL.Invariants     (BS (Balanced, LeftHeavy, RightHeavy),
                                               BalancedState, Height, IsAVL,
                                               US (LeftUnbalanced, NotUnbalanced, RightUnbalanced),
                                               UnbalancedState)
import           Data.Tree.BST.Invariants     (GtN, IsBST, LtN)
import           Data.Tree.ITree              (Tree (EmptyTree, ForkTree))
import           Data.Tree.Node               (Node)
import           Data.Type.Equality           ((:~:) (Refl), gcastWith)
import           GHC.TypeNats                 (Nat)
import           Prelude                      (Bool (True))
import           Unsafe.Coerce                (unsafeCoerce)


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
instance ProofIsBSTBalance' ('ForkTree l (Node n a) r) 'NotUnbalanced where
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
-- | LtN ll n ~ 'True, CmpNat ln n ~ 'LT
instance ProofLtNRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced 'LeftHeavy where
  proofLtNRotate _ _ _ _ = unsafeCoerce Refl
-- | LtN ll n ~ 'True, CmpNat ln n ~ 'LT
instance ProofLtNRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced 'Balanced where
  proofLtNRotate _ _ _ _ = unsafeCoerce Refl

-- | Right-Right case (Left rotation)
-- | CmpNat rn n ~ 'LT, LtN rr n ~ 'True
instance ProofLtNRotate ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced 'RightHeavy where
  proofLtNRotate _ _ _ _ = unsafeCoerce Refl
-- | CmpNat rn n ~ 'LT, LtN rr n ~ 'True
instance ProofLtNRotate ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced 'Balanced where
  proofLtNRotate _ _ _ _ = unsafeCoerce Refl

-- | Left-Right case (First left rotation, then right rotation)
-- | CmpNat ln n ~ 'LT, CmpNat lrn n ~ 'LT, LtN ll n ~ 'True, LtN lrl n ~ 'True
instance ProofLtNRotate ('ForkTree ('ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr)) (Node n1 a) r) n 'LeftUnbalanced 'RightHeavy where
  proofLtNRotate _ _ _ _ = unsafeCoerce Refl

-- | Right-Left case (First right rotation, then left rotation)
-- | CmpNat rln n ~ 'LT, LtN rlr n ~ 'True, LtN rr n ~ 'True, CmpNat rn n ~ 'LT
instance ProofLtNRotate ('ForkTree l (Node n1 a) ('ForkTree ('ForkTree rll (Node rln rla) rlr) (Node rn ra) rr)) n 'RightUnbalanced 'LeftHeavy where
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
-- | CmpNat ln n ~ 'GT, GtN ll n ~ 'True
instance ProofGtNRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced 'LeftHeavy where
  proofGtNRotate _ _ _ _ = unsafeCoerce Refl
-- | GtN ll n ~ 'True, CmpNat ln n ~ 'GT
instance ProofGtNRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced 'Balanced where
  proofGtNRotate _ _ _ _ = unsafeCoerce Refl

-- | Right-Right case (Left rotation)
-- | CmpNat rn n ~ 'GT, GtN rr n ~ 'True
instance ProofGtNRotate ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced 'RightHeavy where
  proofGtNRotate _ _ _ _ = unsafeCoerce Refl
-- | CmpNat rn n ~ 'GT, GtN rr n ~ 'True
instance ProofGtNRotate ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced 'Balanced where
  proofGtNRotate _ _ _ _ = unsafeCoerce Refl

-- | Left-Right case (First left rotation, then right rotation)
-- | CmpNat ln n ~ 'GT, CmpNat lrn n ~ 'GT, GtN ll n ~ 'True, GtN lrl n ~ 'True
instance ProofGtNRotate ('ForkTree ('ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr)) (Node n1 a) r) n 'LeftUnbalanced 'RightHeavy where
  proofGtNRotate _ _ _ _ = unsafeCoerce Refl

-- | Right-Left case (First right rotation, then left rotation)
-- | CmpNat rln n ~ 'GT, GtN rlr n ~ 'True, GtN rr n ~ 'True, CmpNat rn n ~ 'GT
instance ProofGtNRotate ('ForkTree l (Node n1 a) ('ForkTree ('ForkTree rll (Node rln rla) rlr) (Node rn ra) rr)) n 'RightUnbalanced 'LeftHeavy where
  proofGtNRotate _ _ _ _ = unsafeCoerce Refl


-- | Prove that applying a rebalancing (a composition of rotations)
-- | to an almost AVL tree returns an AVL tree.
-- | An almost AVL tree is a tree `t ~ 'ForkTree l (Node n a) r` which verifies
-- | 'IsAVL l ~ 'True && IsAVL r ~ 'True && LtN l n ~ 'True && GtN r n ~ 'True'.
class ProofIsAVLBalance (t :: Tree) where
  proofIsAVLBalance :: Proxy t -> IsAVL (Balance t) :~: 'True
instance ProofIsAVLBalance 'EmptyTree where
  proofIsAVLBalance _ = Refl
instance (IsAVL l ~ 'True, IsAVL r ~ 'True, LtN l n ~ 'True, GtN r n ~ 'True,
  ProofIsAVLBalance' ('ForkTree l (Node n a) r) (UnbalancedState (Height l) (Height r))) =>
  ProofIsAVLBalance ('ForkTree l (Node n a) r) where
  proofIsAVLBalance pt = gcastWith (proofIsAVLBalance' pt (Proxy::Proxy (UnbalancedState (Height l) (Height r)))) Refl

-- | Prove that applying a rebalancing (a composition of rotations)
-- | to an almost AVL tree returns an AVL, given the comparison 'us' of the heights of the left and right sub trees.
-- | An almost AVL tree is a tree `t ~ 'ForkTree l (Node n a) r` which verifies
-- | 'IsAVL l ~ 'True && IsAVL r ~ 'True && LtN l n ~ 'True && GtN r n ~ 'True'.
-- | This is called only from ProofIsAVLBalance.
-- | The 'us' parameter guides the proof.
class ProofIsAVLBalance' (t :: Tree) (us::US) where
  proofIsAVLBalance' :: (t ~ 'ForkTree l (Node n a) r, IsAVL l ~ 'True, IsAVL r ~ 'True, LtN l n ~ 'True, GtN r n ~ 'True) =>
    Proxy t -> Proxy us -> IsAVL (Balance' t us) :~: 'True
-- | BalancedHeights (Height l) (Height r) ~ 'True
instance ProofIsAVLBalance' ('ForkTree l (Node n a) r) 'NotUnbalanced where
  proofIsAVLBalance' _ _ = unsafeCoerce Refl
instance (ProofIsAVLRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced (BalancedState (Height ll) (Height lr))) =>
  ProofIsAVLBalance' ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced where
  proofIsAVLBalance' pt pus = gcastWith (proofIsAVLRotate pt pus (Proxy::Proxy (BalancedState (Height ll) (Height lr)))) Refl
instance (ProofIsAVLRotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced (BalancedState (Height rl) (Height rr))) =>
  ProofIsAVLBalance' ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced where
  proofIsAVLBalance' pt pus = gcastWith (proofIsAVLRotate pt pus (Proxy::Proxy (BalancedState (Height rl) (Height rr)))) Refl


-- | Prove that applying a rotation
-- | to an almost AVL tree returns an AVL tree.
-- | An almost AVL tree is a tree `t ~ 'ForkTree l (Node n a) r` which verifies
-- | 'IsAVL l ~ 'True && IsAVL r ~ 'True && LtN l n ~ 'True && GtN r n ~ 'True'.
-- | Each instance needs some set of hypotesis. However, all these are deduced from the facts
-- | that 'IsBST t ~ 'True' and 'IsAVL t ~ 'True'.
-- | However, the compiler is not able to infer this. Instead of requesting these hypotesis in the context of
-- | every instance (which would increase the computational effort), unsafeCoerce is used.
-- | The hypotesis that the compiler needs are commented within the code.
class ProofIsAVLRotate (t :: Tree) (us::US) (bs::BS) where
  proofIsAVLRotate :: (t ~ 'ForkTree l (Node n a) r, IsAVL l ~ 'True, IsAVL r ~ 'True, LtN l n ~ 'True, GtN r n ~ 'True) =>
    Proxy t -> Proxy us -> Proxy bs -> IsAVL (Rotate t us bs) :~: 'True
-- | Left-Left case (Right rotation)
-- | IsAVL ll ~ 'True, IsAVL lr ~ 'True, BalancedHeights (Height lr) (Height r) ~ 'True,
-- | BalancedHeights (Height ll) (1 + If (Height lr <=? Height r) (Height r) (Height lr)) ~ 'True
instance ProofIsAVLRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'LeftHeavy where
  proofIsAVLRotate _ _ _ = unsafeCoerce Refl
-- | IsAVL ll ~ 'True, IsAVL lr ~ 'True, BalancedHeights (Height lr) (Height r) ~ 'True,
-- | BalancedHeights (Height ll) (1 + If (Height lr <=? Height r) (Height r) (Height lr)) ~ 'True
instance ProofIsAVLRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'Balanced where
  proofIsAVLRotate _ _ _ = unsafeCoerce Refl

-- | Right-Right case (Left rotation)
-- | IsAVL rl ~ 'True, IsAVL rr ~ 'True, BalancedHeights (Height l) (Height rl) ~ 'True,
-- | BalancedHeights (1 + If (Height l <=? Height rl) (Height rl) (Height l)) (Height rr) ~ 'True
instance ProofIsAVLRotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'RightHeavy where
  proofIsAVLRotate _ _ _ = unsafeCoerce Refl
-- | IsAVL rl ~ 'True, IsAVL rr ~ 'True, BalancedHeights (Height l) (Height rl) ~ 'True,
-- | BalancedHeights (1 + If (Height l <=? Height rl) (Height rl) (Height l)) (Height rr) ~ 'True
instance ProofIsAVLRotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'Balanced where
  proofIsAVLRotate _ _ _ = unsafeCoerce Refl

-- | Left-Right case (First left rotation, then right rotation)
-- | IsAVL ll ~ 'True, IsAVL lrl ~ 'True, IsAVL lrr ~ 'True,
-- | BalancedHeights (Height ll) (Height lrl) ~ 'True, BalancedHeights (Height lrr) (Height r) ~ 'True,
-- | BalancedHeights (1 + If (Height ll <=? Height lrl) (Height lrl) (Height ll)) (1 + If (Height lrr <=? Height r) (Height r) (Height lrr)) ~ 'True
instance ProofIsAVLRotate ('ForkTree ('ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr)) (Node n a) r) 'LeftUnbalanced 'RightHeavy where
  proofIsAVLRotate _ _ _ = unsafeCoerce Refl

-- | Right-Left case (First right rotation, then left rotation)
-- | IsAVL rll ~ 'True, IsAVL rlr ~ 'True, IsAVL rr ~ 'True,
-- | BalancedHeights (Height l) (Height rll) ~ 'True, BalancedHeights (Height rlr) (Height rr) ~ 'True
-- | BalancedHeights (1 + If (Height l <=? Height rll) (Height rll) (Height l)) (1 + If (Height rlr <=? Height rr) (Height rr) (Height rlr)) ~ 'True
instance ProofIsAVLRotate ('ForkTree l (Node n a) ('ForkTree ('ForkTree rll (Node rln rla) rlr) (Node rn ra) rr)) 'RightUnbalanced 'LeftHeavy where
  proofIsAVLRotate _ _ _ = unsafeCoerce Refl
