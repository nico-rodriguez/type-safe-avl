{-# OPTIONS_HADDOCK ignore-exports #-}

{-|
Module      : Data.Tree.AVL.Intern.Balance
Description : Balancing algorithm (with proofs) for internalist AVL trees
Copyright   : (c) Nicolás Rodríguez, 2021
License     : GPL-3
Maintainer  : Nicolás Rodríguez
Stability   : experimental
Portability : POSIX

Implementation of the balancing algorithm over internalist AlmostAVL trees,
along with the necessary proofs to ensure (at compile time) that the
new tree is AVL (the balance is restored).
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# LANGUAGE Safe                  #-}

module Data.Tree.AVL.Intern.Balance (
  Balanceable(Balance,balance),
  ProofLtNBalance(proofLtNBalance),
  ProofGtNBalance(proofGtNBalance)
) where

import           Data.Proxy                       (Proxy (Proxy))
import           Data.Tree.AVL.Intern.Constructors (AVL (ForkAVL), AlmostAVL (AlmostAVL))
import           Data.Tree.AVL.Invariants         (BS (Balanced, LeftHeavy, RightHeavy),
                                                   BalancedHeights, BalancedState, Height,
                                                   US (LeftUnbalanced, NotUnbalanced, RightUnbalanced),
                                                   UnbalancedState)
import           Data.Tree.BST.Invariants         (GtN, LtN)
import           Data.Tree.ITree                  (Tree (ForkTree))
import           Data.Tree.Node                   (Node)
import           Data.Type.Equality               ((:~:) (Refl), gcastWith)
import           GHC.TypeLits                     (type (+), type (<=?), Nat, CmpNat)
import           Prelude                          (Bool (True), Ordering (LT, GT))


-- | This type class provides the functionality to balance
-- an `AlmostAVL` 't' (a tree that came up from an insertion or deletion
-- on an `AVL`).
-- The insertion is defined at the value level and the type level.
class Balanceable (t :: Tree) where
  type Balance (t :: Tree) :: Tree
  balance :: AlmostAVL t -> AVL (Balance t)
instance (us ~ UnbalancedState (Height l) (Height r),
  Balanceable' ('ForkTree l (Node n a) r) us) =>
  Balanceable ('ForkTree l (Node n a) r) where
  type Balance ('ForkTree l (Node n a) r) = Balance' ('ForkTree l (Node n a) r) (UnbalancedState (Height l) (Height r))
  balance t = balance' t (Proxy::Proxy us)

-- | This type class provides the functionality to balance
-- an `AlmostAVL` 't'.
-- It's only used by the 'Balanceable' class and it has one extra parameter 'us',
-- which is the `UnbalancedState` of the two sub trees of 't'.
class Balanceable' (t :: Tree) (us :: US) where
  type Balance' (t :: Tree) (us :: US) :: Tree
  balance' :: AlmostAVL t -> Proxy us -> AVL (Balance' t us)
instance (BalancedHeights (Height l) (Height r) ~ 'True) =>
  Balanceable' ('ForkTree l (Node n a) r) 'NotUnbalanced where
  type Balance' ('ForkTree l (Node n a) r) 'NotUnbalanced = 'ForkTree l (Node n a) r
  balance' (AlmostAVL l node r) _ = ForkAVL l node r
instance (bs ~ BalancedState (Height ll) (Height lr),
  Rotateable ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced bs) =>
  Balanceable' ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced where
  type Balance' ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced =
    Rotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced (BalancedState (Height ll) (Height lr))
  balance' t pus = rotate t pus (Proxy::Proxy bs)
instance (bs ~ BalancedState (Height rl) (Height rr),
  Rotateable ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced bs) =>
  Balanceable' ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced where
  type Balance' ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced =
    Rotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced (BalancedState (Height rl) (Height rr))
  balance' t pus = rotate t pus (Proxy::Proxy bs)


-- | This type class provides the functionality to apply a rotation to
-- an `AlmostAVL` tree 't'.
-- The rotation is defined at the value level and the type level.
class Rotateable (t :: Tree) (us :: US) (bs :: BS) where
  type Rotate (t :: Tree) (us :: US) (bs :: BS) :: Tree
  rotate :: AlmostAVL t -> Proxy us -> Proxy bs -> AVL (Rotate t us bs)
-- Left-Left case (Right rotation)
instance (l ~ 'ForkTree ll (Node ln la) lr,
  CmpNat n ln ~ 'GT, GtN r ln ~ 'True, LtN lr n ~ 'True, (Height lr <=? Height r) ~ 'True,
  BalancedHeights (Height ll) (1 + Height r) ~ 'True, BalancedHeights (Height lr) (Height r) ~ 'True) =>
  Rotateable ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'LeftHeavy where
  type Rotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'LeftHeavy =
    'ForkTree ll (Node ln la) ('ForkTree lr (Node n a) r)
  rotate (AlmostAVL (ForkAVL ll lnode lr) xnode r) _ _ =
    ForkAVL ll lnode (ForkAVL lr xnode r)
instance (l ~ 'ForkTree ll (Node ln la) lr,
  CmpNat n ln ~ 'GT, GtN r ln ~ 'True, LtN lr n ~ 'True, (Height lr <=? Height r) ~ 'True,
  BalancedHeights (Height ll) (1 + Height r) ~ 'True, BalancedHeights (Height lr) (Height r) ~ 'True) =>
  Rotateable ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'Balanced where
  type Rotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'Balanced =
    'ForkTree ll (Node ln la) ('ForkTree lr (Node n a) r)
  rotate (AlmostAVL (ForkAVL ll lnode lr) xnode r) _ _ =
    ForkAVL ll lnode (ForkAVL lr xnode r)
-- Right-Right case (Left rotation)
instance (r ~ 'ForkTree rl (Node rn ra) rr,
  (Height l <=? Height rl) ~ 'True, CmpNat n rn ~ 'LT, LtN l rn ~ 'True, GtN rl n ~ 'True,
  BalancedHeights (1 + Height rl) (Height rr) ~ 'True, BalancedHeights (Height l) (Height rl) ~ 'True) =>
  Rotateable ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'RightHeavy where
  type Rotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'RightHeavy =
    'ForkTree ('ForkTree l (Node n a) rl) (Node rn ra) rr
  rotate (AlmostAVL l xnode (ForkAVL rl rnode rr)) _ _ =
    ForkAVL (ForkAVL l xnode rl) rnode rr
instance (r ~ 'ForkTree rl (Node rn ra) rr,
  (Height l <=? Height rl) ~ 'True, CmpNat n rn ~ 'LT, LtN l rn ~ 'True, GtN rl n ~ 'True,
  BalancedHeights (1 + Height rl) (Height rr) ~ 'True, BalancedHeights (Height l) (Height rl) ~ 'True) =>
  Rotateable ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'Balanced where
  type Rotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'Balanced =
    'ForkTree ('ForkTree l (Node n a) rl) (Node rn ra) rr
  rotate (AlmostAVL l xnode (ForkAVL rl rnode rr)) _ _ =
    ForkAVL (ForkAVL l xnode rl) rnode rr
-- Left-Right case (First left rotation, then right rotation)
instance (l ~ 'ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr),
  (Height ll <=? Height lrl) ~ 'True, GtN r lrn ~ 'True, LtN ll lrn ~ 'True, LtN lrr n ~ 'True,
  (Height lrr <=? Height r) ~ 'True, CmpNat n lrn ~ 'GT, CmpNat ln lrn ~ 'LT, GtN lrl ln ~ 'True,
  BalancedHeights (1 + Height lrl) (1 + Height r) ~ 'True, BalancedHeights (Height ll) (Height lrl) ~ 'True,
  BalancedHeights (Height lrr) (Height r) ~ 'True) =>
  Rotateable ('ForkTree ('ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr)) (Node n a) r) 'LeftUnbalanced 'RightHeavy where
  type Rotate ('ForkTree ('ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr)) (Node n a) r) 'LeftUnbalanced 'RightHeavy =
    'ForkTree ('ForkTree ll (Node ln la) lrl) (Node lrn lra) ('ForkTree lrr (Node n a) r)
  rotate (AlmostAVL (ForkAVL ll lnode (ForkAVL lrl lrnode lrr)) xnode r) _ _ =
    ForkAVL (ForkAVL ll lnode lrl) lrnode (ForkAVL lrr xnode r)
-- Right-Left case (First right rotation, then left rotation)
instance (r ~ 'ForkTree ('ForkTree rll (Node rln rla) rlr) (Node rn ra) rr,
  (Height l <=? Height rll) ~ 'True, CmpNat rn rln ~ 'GT, CmpNat n rln ~ 'LT, LtN l rln ~ 'True,
  (Height rlr <=? Height rr) ~ 'True, GtN rr rln ~ 'True, GtN rll n ~ 'True, LtN rlr rn ~ 'True,
  BalancedHeights (1 + Height rll) (1 + Height rr) ~ 'True, BalancedHeights (Height l) (Height rll) ~ 'True,
  BalancedHeights (Height rlr) (Height rr) ~ 'True) =>
  Rotateable ('ForkTree l (Node n a) ('ForkTree ('ForkTree rll (Node rln rla) rlr) (Node rn ra) rr)) 'RightUnbalanced 'LeftHeavy where
  type Rotate ('ForkTree l (Node n a) ('ForkTree ('ForkTree rll (Node rln rla) rlr) (Node rn ra) rr)) 'RightUnbalanced 'LeftHeavy =
    'ForkTree ('ForkTree l (Node n a) rll) (Node rln rla) ('ForkTree rlr (Node rn ra) rr)
  rotate (AlmostAVL l xnode (ForkAVL (ForkAVL rll rlnode rlr) rnode rr)) _ _ =
    ForkAVL (ForkAVL l xnode rll) rlnode (ForkAVL rlr rnode rr)


-- | Prove that rebalancing a tree 't' which verifies @LtN t n ~ 'True@ preserves the `LtN` invariant.
class ProofLtNBalance (t :: Tree) (n :: Nat) where
  proofLtNBalance :: (LtN t n ~ 'True) =>
    AlmostAVL t -> Proxy n -> LtN (Balance t) n :~: 'True
instance (us ~ UnbalancedState (Height l) (Height r), ProofLtNBalance' ('ForkTree l (Node n1 a) r) n us) =>
  ProofLtNBalance ('ForkTree l (Node n1 a) r) n where
  proofLtNBalance pt pn = gcastWith (proofLtNBalance' pt pn (Proxy::Proxy us)) Refl

-- | Prove that rebalancing a tree 't' which verifies @LtN t n ~ 'True@ preserves the `LtN` invariant,
-- given the `UnbalancedState` 'us' of the tree.
-- The 'us' parameter guides the proof.
class ProofLtNBalance' (t :: Tree) (n :: Nat) (us :: US) where
  proofLtNBalance' :: (LtN t n ~ 'True) =>
    AlmostAVL t -> Proxy n -> Proxy us -> LtN (Balance' t us) n :~: 'True
instance ProofLtNBalance' ('ForkTree l (Node n1 a) r) n 'NotUnbalanced where
  proofLtNBalance' _ _ _ = Refl
instance (bs ~ BalancedState (Height ll) (Height lr),
  ProofLtNRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced bs) =>
  ProofLtNBalance' ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced where
  proofLtNBalance' pt pn pus = gcastWith (proofLtNRotate pt pn pus (Proxy::Proxy bs)) Refl
instance (bs ~ BalancedState (Height rl) (Height rr),
  ProofLtNRotate ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced bs) =>
  ProofLtNBalance' ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced where
  proofLtNBalance' pt pn pus = gcastWith (proofLtNRotate pt pn pus (Proxy::Proxy bs)) Refl


-- | Prove that applying a rotation to a tree 't' which verifies @LtN t n ~ 'True@ preserves the `LtN` invariant.
class ProofLtNRotate (t :: Tree) (n :: Nat) (us :: US) (bs :: BS) where
  proofLtNRotate :: (LtN t n ~ 'True) =>
    AlmostAVL t -> Proxy n -> Proxy us -> Proxy bs -> LtN (Rotate t us bs) n :~: 'True

-- Left-Left case (Right rotation)
instance (LtN ll n ~ 'True, CmpNat ln n ~ 'LT,  LtN lr n ~ 'True, LtN r n ~ 'True) =>
  ProofLtNRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced 'LeftHeavy where
  proofLtNRotate _ _ _ _ = Refl
instance (LtN ll n ~ 'True, CmpNat ln n ~ 'LT,  LtN lr n ~ 'True, LtN r n ~ 'True) =>
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
    AlmostAVL t -> Proxy n -> GtN (Balance t) n :~: 'True
instance (us ~ UnbalancedState (Height l) (Height r),
  ProofGtNBalance' ('ForkTree l (Node n1 a) r) n us) =>
  ProofGtNBalance ('ForkTree l (Node n1 a) r) n where
  proofGtNBalance t pn = gcastWith (proofGtNBalance' t pn (Proxy::Proxy us)) Refl

-- | Prove that rebalancing a tree 't' which verifies @GtN t n ~ 'True@ preserves the `GtN` invariant,
-- given the `UnbalancedState` 'us' of the tree.
-- The 'us' parameter guides the proof.
class ProofGtNBalance' (t :: Tree) (n :: Nat) (us :: US) where
  proofGtNBalance' :: (GtN t n ~ 'True) =>
    AlmostAVL t -> Proxy n -> Proxy us -> GtN (Balance' t us) n :~: 'True
instance ProofGtNBalance' ('ForkTree l (Node n1 a) r) n 'NotUnbalanced where
  proofGtNBalance' _ _ _ = Refl
instance (bs ~ BalancedState (Height ll) (Height lr),
  ProofGtNRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced bs) =>
  ProofGtNBalance' ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced where
  proofGtNBalance' t pn pus = gcastWith (proofGtNRotate t pn pus (Proxy::Proxy bs)) Refl
instance (bs ~ BalancedState (Height rl) (Height rr),
  ProofGtNRotate ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced bs) =>
  ProofGtNBalance' ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced where
  proofGtNBalance' t pn pus = gcastWith (proofGtNRotate t pn pus (Proxy::Proxy bs)) Refl


-- | Prove that applying a rotation to a tree 't' which verifies @GtN t n ~ 'True@ preserves the `GtN` invariant.
class ProofGtNRotate (t :: Tree) (n :: Nat) (us :: US) (bs :: BS) where
  proofGtNRotate :: (GtN t n ~ 'True) =>
    AlmostAVL t -> Proxy n -> Proxy us -> Proxy bs -> GtN (Rotate t us bs) n :~: 'True

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
