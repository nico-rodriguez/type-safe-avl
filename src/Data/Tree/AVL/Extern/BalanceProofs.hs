{-# OPTIONS_HADDOCK ignore-exports #-}

{-|
Module      : Data.Tree.AVL.Extern.BalanceProofs
Description : Proofs for balance operations for externalist AVL trees
Copyright   : (c) Nicolás Rodríguez, 2021
License     : GPL-3
Maintainer  : Nicolás Rodríguez
Stability   : experimental
Portability : POSIX

Implementation of the necessary proofs to ensure (at compile time) that the
balance algorithm defined in "Data.Tree.AVL.Extern.Balance" respects the key ordering and recovers the height balance.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# LANGUAGE Trustworthy           #-}

module Data.Tree.AVL.Extern.BalanceProofs (
  ProofIsBSTBalance(proofIsBSTBalance),
  ProofLtNBalance(proofLtNBalance),
  ProofGtNBalance(proofGtNBalance),
  ProofIsAVLBalance(proofIsAVLBalance)
) where

import           Data.Proxy                     (Proxy (Proxy))
import           Data.Tree.AVL.Extern.Balance   (Balanceable (Balance),
                                                 Balanceable' (Balance'),
                                                 Rotateable (Rotate))
import           Data.Tree.AVL.Invariants       (BS (Balanced, LeftHeavy, RightHeavy),
                                                 BalancedState, Height,
                                                 US (LeftUnbalanced, NotUnbalanced, RightUnbalanced),
                                                 UnbalancedState, BalancedHeights)
import           Data.Tree.AVL.Extern.Constructors (IsAVLT(EmptyIsAVLT,ForkIsAVLT), IsAlmostAVLT(ForkIsAlmostAVLT))
import           Data.Tree.BST.Invariants       (GtN, LtN)
import           Data.Tree.BST.Extern.Constructors (IsBSTT(EmptyIsBSTT,ForkIsBSTT))
import           Data.Tree.ITree                (Tree (EmptyTree, ForkTree))
import           Data.Tree.Node                 (Node)
import           Data.Type.Equality             ((:~:) (Refl), gcastWith)
import           GHC.TypeNats                   (type (<=?), type (+), Nat, CmpNat)
import           Prelude                        (Bool (True), Ordering (LT, GT))


-- | Prove that applying a rebalancing (a composition of rotations)
-- to a `BST` tree preserves `BST` condition.
-- The `BST` invariant was already checked since this proof is called after proofs for `Insert` or `Delete`.
class ProofIsBSTBalance (t :: Tree) where
  proofIsBSTBalance :: IsBSTT t -> IsBSTT (Balance t)
instance ProofIsBSTBalance 'EmptyTree where
  proofIsBSTBalance _ = EmptyIsBSTT
instance (us ~ UnbalancedState (Height l) (Height r), ProofIsBSTBalance' ('ForkTree l (Node n a) r) us) =>
  ProofIsBSTBalance ('ForkTree l (Node n a) r) where
  proofIsBSTBalance tIsBST = proofIsBSTBalance' tIsBST (Proxy::Proxy us)

-- | Prove that applying a rebalancing (a composition of rotations)
-- to a `BST` tree preserves `BST` condition, given the comparison 'us' of the heights of the left and right sub trees.
-- This is called only from `ProofIsBSTBalance`.
-- The `BST` invariant was already checked since this proof is called after proofs for `Insert` or `Delete`.
class ProofIsBSTBalance' (t :: Tree) (us :: US) where
  proofIsBSTBalance' :: IsBSTT t -> Proxy us -> IsBSTT (Balance' t us)
instance ProofIsBSTBalance' ('ForkTree l (Node n a) r) 'NotUnbalanced where
  proofIsBSTBalance' tIsBST _ = tIsBST
instance (bs ~ BalancedState (Height ll) (Height lr),
  ProofIsBSTRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced bs) =>
  ProofIsBSTBalance' ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced where
  proofIsBSTBalance' tIsBST pus = proofIsBSTRotate tIsBST pus (Proxy::Proxy bs)
instance (bs ~ BalancedState (Height rl) (Height rr),
  ProofIsBSTRotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced bs) =>
  ProofIsBSTBalance' ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced where
  proofIsBSTBalance' tIsBST pus = proofIsBSTRotate tIsBST pus (Proxy::Proxy bs)


-- | Prove that applying a rotation
-- to a `BST` tree preserves `BST` condition.
-- The `BST` invariant was already checked since this proof is called after proofs for `Insert` or `Delete`.
class ProofIsBSTRotate (t :: Tree) (us :: US) (bs :: BS) where
  proofIsBSTRotate :: IsBSTT t -> Proxy us -> Proxy bs -> IsBSTT (Rotate t us bs)

-- Left-Left case (Right rotation)
instance (l ~ 'ForkTree ll (Node ln la) lr,
  CmpNat n ln ~ 'GT, GtN r ln ~ 'True, LtN lr n ~ 'True) =>
  ProofIsBSTRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'LeftHeavy where
  proofIsBSTRotate (ForkIsBSTT (ForkIsBSTT ll lnode lr) node r) _ _ =
    ForkIsBSTT ll lnode (ForkIsBSTT lr node r)
instance (l ~ 'ForkTree ll (Node ln la) lr,
  CmpNat n ln ~ 'GT, GtN r ln ~ 'True, LtN lr n ~ 'True) =>
  ProofIsBSTRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'Balanced where
  proofIsBSTRotate (ForkIsBSTT (ForkIsBSTT ll lnode lr) node r) _ _ =
    ForkIsBSTT ll lnode (ForkIsBSTT lr node r)

-- Right-Right case (Left rotation)
instance (r ~ 'ForkTree rl (Node rn ra) rr,
  CmpNat n rn ~ 'LT, LtN l rn ~ 'True, GtN rl n ~ 'True) =>
  ProofIsBSTRotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'RightHeavy where
  proofIsBSTRotate (ForkIsBSTT l node (ForkIsBSTT rl rnode rr)) _ _ =
    ForkIsBSTT (ForkIsBSTT l node rl) rnode rr
instance (r ~ 'ForkTree rl (Node rn ra) rr,
  CmpNat n rn ~ 'LT, LtN l rn ~ 'True, GtN rl n ~ 'True) =>
  ProofIsBSTRotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'Balanced where
  proofIsBSTRotate (ForkIsBSTT l node (ForkIsBSTT rl rnode rr)) _ _ =
    ForkIsBSTT (ForkIsBSTT l node rl) rnode rr

-- Left-Right case (First left rotation, then right rotation)
instance (lr ~ 'ForkTree lrl (Node lrn lra) lrr, l ~ 'ForkTree ll (Node ln la) lr,
  CmpNat n lrn ~ 'GT, GtN r lrn ~ 'True, CmpNat ln lrn ~ 'LT, LtN ll lrn ~ 'True, GtN lrl ln ~ 'True, LtN lrr n ~ 'True) =>
  ProofIsBSTRotate ('ForkTree ('ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr)) (Node n a) r) 'LeftUnbalanced 'RightHeavy where
  proofIsBSTRotate (ForkIsBSTT (ForkIsBSTT ll lnode (ForkIsBSTT lrl lrnode lrr)) node r) _ _ =
    ForkIsBSTT (ForkIsBSTT ll lnode lrl) lrnode (ForkIsBSTT lrr node r)

-- Right-Left case (First right rotation, then left rotation)
instance (rl ~ 'ForkTree rll (Node rln rla) rlr, r ~ 'ForkTree rl (Node rn ra) rr,
  CmpNat rn rln ~ 'GT, GtN rr rln ~ 'True, CmpNat n rln ~ 'LT, LtN l rln ~ 'True, GtN rll n ~ 'True, LtN rlr rn ~ 'True) =>
  ProofIsBSTRotate ('ForkTree l (Node n a) ('ForkTree ('ForkTree rll (Node rln rla) rlr) (Node rn ra) rr)) 'RightUnbalanced 'LeftHeavy where
  proofIsBSTRotate (ForkIsBSTT l node (ForkIsBSTT (ForkIsBSTT rll rlnode rlr) rnode rr)) _ _ =
    ForkIsBSTT (ForkIsBSTT l node rll) rlnode (ForkIsBSTT rlr rnode rr)


-- | Prove that rebalancing a tree 't' which verifies @LtN t n ~ 'True@ preserves the `LtN` invariant.
class ProofLtNBalance (t :: Tree) (n :: Nat) where
  proofLtNBalance :: (LtN t n ~ 'True) =>
    IsBSTT t -> Proxy n -> LtN (Balance t) n :~: 'True
instance ProofLtNBalance 'EmptyTree n where
  proofLtNBalance _ _ = Refl
instance (us ~ UnbalancedState (Height l) (Height r),
  ProofLtNBalance' ('ForkTree l (Node n1 a) r) n us) =>
  ProofLtNBalance ('ForkTree l (Node n1 a) r) n where
  proofLtNBalance tIsBST pn = gcastWith (proofLtNBalance' tIsBST pn (Proxy::Proxy us)) Refl

-- | Prove that rebalancing a tree 't' which verifies @LtN t n ~ 'True@ preserves the `LtN` invariant,
-- given the unbalanced state 'us' of the tree.
class ProofLtNBalance' (t :: Tree) (n :: Nat) (us :: US) where
  proofLtNBalance' :: (LtN t n ~ 'True) =>
    IsBSTT t -> Proxy n -> Proxy us -> LtN (Balance' t us) n :~: 'True
instance ProofLtNBalance' ('ForkTree l (Node n1 a) r) n 'NotUnbalanced where
  proofLtNBalance' _ _ _ = Refl
instance (bs ~ BalancedState (Height ll) (Height lr),
  ProofLtNRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced bs) =>
  ProofLtNBalance' ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced where
  proofLtNBalance' tIsBST pn pus = gcastWith (proofLtNRotate tIsBST pn pus (Proxy::Proxy bs)) Refl
instance (bs ~ BalancedState (Height rl) (Height rr),
  ProofLtNRotate ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced bs) =>
  ProofLtNBalance' ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced where
  proofLtNBalance' tIsBST pn pus = gcastWith (proofLtNRotate tIsBST pn pus (Proxy::Proxy bs)) Refl


-- | Prove that applying a rotation to a tree 't' which verifies @LtN t n ~ 'True@ preserves the `LtN` invariant.
class ProofLtNRotate (t :: Tree) (n :: Nat) (us :: US) (bs :: BS) where
  proofLtNRotate :: (LtN t n ~ 'True) =>
    IsBSTT t -> Proxy n -> Proxy us -> Proxy bs -> LtN (Rotate t us bs) n :~: 'True

-- Left-Left case (Right rotation)
instance (LtN ll n ~ 'True, CmpNat ln n ~ 'LT,  LtN lr n ~ 'True, LtN r n ~ 'True) =>
  ProofLtNRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced 'LeftHeavy where
  proofLtNRotate _ _ _ _ = Refl
instance (LtN ll n ~ 'True, CmpNat ln n ~ 'LT, LtN lr n ~ 'True, LtN r n ~ 'True) =>
  ProofLtNRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced 'Balanced where
  proofLtNRotate _ _ _ _ = Refl

-- Right-Right case (Left rotation)
instance (LtN l n ~ 'True, LtN rl n ~ 'True, CmpNat rn n ~ 'LT, LtN rr n ~ 'True) =>
  ProofLtNRotate ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced 'RightHeavy where
  proofLtNRotate _ _ _ _ = Refl
instance (LtN l n ~ 'True, LtN rl n ~ 'True, CmpNat rn n ~ 'LT, LtN rr n ~ 'True) =>
  ProofLtNRotate ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced 'Balanced where
  proofLtNRotate _ _ _ _ = Refl

-- Left-Right case (First left rotation, then right rotation)
instance (LtN ll n ~ 'True, CmpNat ln n ~ 'LT, LtN lrl n ~ 'True, CmpNat lrn n ~ 'LT, LtN lrr n ~ 'True, LtN r n ~ 'True) =>
  ProofLtNRotate ('ForkTree ('ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr)) (Node n1 a) r) n 'LeftUnbalanced 'RightHeavy where
  proofLtNRotate _ _ _ _ = Refl

-- Right-Left case (First right rotation, then left rotation)
instance (LtN l n ~ 'True, LtN rll n ~ 'True, CmpNat rln n ~ 'LT, LtN rlr n ~ 'True, CmpNat rn n ~ 'LT, LtN rr n ~ 'True) =>
  ProofLtNRotate ('ForkTree l (Node n1 a) ('ForkTree ('ForkTree rll (Node rln rla) rlr) (Node rn ra) rr)) n 'RightUnbalanced 'LeftHeavy where
  proofLtNRotate _ _ _ _ = Refl


-- | Prove that rebalancing a tree 't' which verifies @GtN t n ~ 'True@ preserves the `GtN` invariant.
class ProofGtNBalance (t :: Tree) (n :: Nat) where
  proofGtNBalance :: (GtN t n ~ 'True) =>
    IsBSTT t -> Proxy n -> GtN (Balance t) n :~: 'True
instance ProofGtNBalance 'EmptyTree n where
  proofGtNBalance _ _ = Refl
instance (us ~ UnbalancedState (Height l) (Height r),
  ProofGtNBalance' ('ForkTree l (Node n1 a) r) n us) =>
  ProofGtNBalance ('ForkTree l (Node n1 a) r) n where
  proofGtNBalance tIsBST pn = gcastWith (proofGtNBalance' tIsBST pn (Proxy::Proxy us)) Refl

-- | Prove that rebalancing a tree 't' which verifies @GtN t n ~ 'True@ preserves the `GtN` invariant,
-- given the unbalanced state 'us' of the tree.
class ProofGtNBalance' (t :: Tree) (n :: Nat) (us :: US) where
  proofGtNBalance' :: (GtN t n ~ 'True) =>
    IsBSTT t -> Proxy n -> Proxy us -> GtN (Balance' t us) n :~: 'True
instance ProofGtNBalance' ('ForkTree l (Node n1 a) r) n 'NotUnbalanced where
  proofGtNBalance' _ _ _ = Refl
instance (bs ~ BalancedState (Height ll) (Height lr),
  ProofGtNRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced bs) =>
  ProofGtNBalance' ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced where
  proofGtNBalance' tIsBST pn pus = gcastWith (proofGtNRotate tIsBST pn pus (Proxy::Proxy bs)) Refl
instance (bs ~ BalancedState (Height rl) (Height rr),
  ProofGtNRotate ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced bs) =>
  ProofGtNBalance' ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced where
  proofGtNBalance' tIsBST pn pus = gcastWith (proofGtNRotate tIsBST pn pus (Proxy::Proxy bs)) Refl


-- | Prove that applying a rotation to a tree 't' which verifies @GtN t n ~ 'True@ preserves the `GtN` invariant.
class ProofGtNRotate (t :: Tree) (n :: Nat) (us :: US) (bs :: BS) where
  proofGtNRotate :: (GtN t n ~ 'True) =>
    IsBSTT t -> Proxy n -> Proxy us -> Proxy bs -> GtN (Rotate t us bs) n :~: 'True

-- Left-Left case (Right rotation)
instance (GtN ll n ~ 'True, CmpNat ln n ~ 'GT, GtN lr n ~ 'True, GtN r n ~ 'True) =>
  ProofGtNRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced 'LeftHeavy where
  proofGtNRotate _ _ _ _ = Refl
instance (GtN ll n ~ 'True, CmpNat ln n ~ 'GT, GtN lr n ~ 'True, GtN r n ~ 'True) =>
  ProofGtNRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced 'Balanced where
  proofGtNRotate _ _ _ _ = Refl

-- Right-Right case (Left rotation)
instance (GtN l n ~ 'True, GtN rl n ~ 'True, CmpNat rn n ~ 'GT, GtN rr n ~ 'True) =>
  ProofGtNRotate ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced 'RightHeavy where
  proofGtNRotate _ _ _ _ = Refl
instance (GtN l n ~ 'True, GtN rl n ~ 'True, CmpNat rn n ~ 'GT, GtN rr n ~ 'True) =>
  ProofGtNRotate ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced 'Balanced where
  proofGtNRotate _ _ _ _ = Refl

-- Left-Right case (First left rotation, then right rotation)
instance (GtN ll n ~ 'True, CmpNat ln n ~ 'GT, GtN lrl n ~ 'True, CmpNat lrn n ~ 'GT, GtN lrr n ~ 'True, GtN r n ~ 'True) =>
  ProofGtNRotate ('ForkTree ('ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr)) (Node n1 a) r) n 'LeftUnbalanced 'RightHeavy where
  proofGtNRotate _ _ _ _ = Refl

-- Right-Left case (First right rotation, then left rotation)
instance (GtN l n ~ 'True, GtN rll n ~ 'True, CmpNat rln n ~ 'GT, GtN rlr n ~ 'True, CmpNat rn n ~ 'GT, GtN rr n ~ 'True) =>
  ProofGtNRotate ('ForkTree l (Node n1 a) ('ForkTree ('ForkTree rll (Node rln rla) rlr) (Node rn ra) rr)) n 'RightUnbalanced 'LeftHeavy where
  proofGtNRotate _ _ _ _ = Refl


-- | Prove that applying a rebalancing (a composition of rotations)
-- to an `almost AVL` tree returns an `AVL` tree.
--
-- An `almost AVL` tree is a tree @t ~ 'ForkTree l (Node n a) r@ which verifies all the following conditions:
--
--  * @IsAVL l ~ 'True@
--
--  * @IsAVL r ~ 'True@
--
--  * @LtN l n ~ 'True@
--
--  * @GtN r n ~ 'True@
--
-- In other words, it's a BST tree with left and right AVL sub trees that may not be balanced at the root.
class ProofIsAVLBalance (t :: Tree) where
  proofIsAVLBalance :: IsAlmostAVLT t -> IsAVLT (Balance t)
instance ProofIsAVLBalance 'EmptyTree where
  proofIsAVLBalance _ = EmptyIsAVLT
instance (us ~ UnbalancedState (Height l) (Height r),
  LtN l n ~ 'True, GtN r n ~ 'True,
  ProofIsAVLBalance' ('ForkTree l (Node n a) r) us) =>
  ProofIsAVLBalance ('ForkTree l (Node n a) r) where
  proofIsAVLBalance tIsAlmostAVL = proofIsAVLBalance' tIsAlmostAVL (Proxy::Proxy us)

-- | Prove that applying a rebalancing (a composition of rotations)
-- to an `almost AVL` tree returns an `AVL`, given the comparison 'us' of the heights of the left and right sub trees.
-- This is called only from `ProofIsAVLBalance`.
class ProofIsAVLBalance' (t :: Tree) (us :: US) where
  proofIsAVLBalance' :: (t ~ 'ForkTree l (Node n a) r, LtN l n ~ 'True, GtN r n ~ 'True) =>
    IsAlmostAVLT t -> Proxy us -> IsAVLT (Balance' t us)
-- | NotUnbalanced implies BalancedHeights (Height l) (Height r) ~ 'True
instance (BalancedHeights (Height l) (Height r) ~ 'True) => ProofIsAVLBalance' ('ForkTree l (Node n a) r) 'NotUnbalanced where
  proofIsAVLBalance' (ForkIsAlmostAVLT l node r) _ = ForkIsAVLT l node r
instance (bs ~ BalancedState (Height ll) (Height lr),
  ProofIsAVLRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced bs) =>
  ProofIsAVLBalance' ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced where
  proofIsAVLBalance' tIsAlmostAVL pus = proofIsAVLRotate tIsAlmostAVL pus (Proxy::Proxy bs)
instance (bs ~ BalancedState (Height rl) (Height rr),
  ProofIsAVLRotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced bs) =>
  ProofIsAVLBalance' ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced where
  proofIsAVLBalance' tIsAlmostAVL pus = proofIsAVLRotate tIsAlmostAVL pus (Proxy::Proxy bs)


-- | Prove that applying a rotation
-- to an `almost AVL` tree returns an `AVL` tree.
class ProofIsAVLRotate (t :: Tree) (us :: US) (bs :: BS) where
  proofIsAVLRotate :: (t ~ 'ForkTree l (Node n a) r, LtN l n ~ 'True, GtN r n ~ 'True) =>
    IsAlmostAVLT t -> Proxy us -> Proxy bs -> IsAVLT (Rotate t us bs)
-- Left-Left case (Right rotation)
instance ((Height lr <=? Height r) ~ 'True, BalancedHeights (Height ll) (1 + Height r) ~ 'True,
  BalancedHeights (Height lr) (Height r) ~ 'True) =>
  ProofIsAVLRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'LeftHeavy where
  proofIsAVLRotate (ForkIsAlmostAVLT (ForkIsAVLT ll lnode lr) node r) _ _ =
    ForkIsAVLT ll lnode (ForkIsAVLT lr node r)
instance ((Height lr <=? Height r) ~ 'True, BalancedHeights (Height ll) (1 + Height r) ~ 'True,
  BalancedHeights (Height lr) (Height r) ~ 'True) =>
  ProofIsAVLRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'Balanced where
  proofIsAVLRotate (ForkIsAlmostAVLT (ForkIsAVLT ll lnode lr) node r) _ _ =
    ForkIsAVLT ll lnode (ForkIsAVLT lr node r)

-- Right-Right case (Left rotation)
instance ((Height l <=? Height rl) ~ 'True, BalancedHeights (1 + Height rl) (Height rr) ~ 'True,
  BalancedHeights (Height l) (Height rl) ~ 'True) =>
  ProofIsAVLRotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'RightHeavy where
  proofIsAVLRotate (ForkIsAlmostAVLT l node (ForkIsAVLT rl rnode rr)) _ _ =
    ForkIsAVLT (ForkIsAVLT l node rl) rnode rr
instance ((Height l <=? Height rl) ~ 'True, BalancedHeights (1 + Height rl) (Height rr) ~ 'True,
  BalancedHeights (Height l) (Height rl) ~ 'True) =>
  ProofIsAVLRotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'Balanced where
  proofIsAVLRotate (ForkIsAlmostAVLT l node (ForkIsAVLT rl rnode rr)) _ _ =
    ForkIsAVLT (ForkIsAVLT l node rl) rnode rr

-- Left-Right case (First left rotation, then right rotation)
instance ((Height ll <=? Height lrl) ~ 'True, BalancedHeights (1 + Height lrl) (1 + Height r) ~ 'True,
  (Height lrr <=? Height r) ~ 'True, BalancedHeights (Height lrr) (Height r) ~ 'True,
  BalancedHeights (Height ll) (Height lrl) ~ 'True) =>
  ProofIsAVLRotate ('ForkTree ('ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr)) (Node n a) r) 'LeftUnbalanced 'RightHeavy where
  proofIsAVLRotate (ForkIsAlmostAVLT (ForkIsAVLT ll lnode (ForkIsAVLT lrl lrnode lrr)) node r) _ _ =
    ForkIsAVLT (ForkIsAVLT ll lnode lrl) lrnode (ForkIsAVLT lrr node r)

-- Right-Left case (First right rotation, then left rotation)
instance ((Height l <=? Height rll) ~ 'True, BalancedHeights (1 + Height rll) (1 + Height rr) ~ 'True,
  (Height rlr <=? Height rr) ~ 'True, BalancedHeights (Height rlr) (Height rr) ~ 'True,
  BalancedHeights (Height l) (Height rll) ~ 'True) =>
  ProofIsAVLRotate ('ForkTree l (Node n a) ('ForkTree ('ForkTree rll (Node rln rla) rlr) (Node rn ra) rr)) 'RightUnbalanced 'LeftHeavy where
  proofIsAVLRotate (ForkIsAlmostAVLT l node (ForkIsAVLT (ForkIsAVLT rll rlnode rlr) rnode rr)) _ _ =
    ForkIsAVLT (ForkIsAVLT l node rll) rlnode (ForkIsAVLT rlr rnode rr)
