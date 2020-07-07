{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Tree.AVL.Intern.Balance (
  Balanceable(Balance,balance),
  ProofLtNBalance(proofLtNBalance),
  ProofGtNBalance(proofGtNBalance)
) where

import           Data.NatProofs                   (proofGTLT, proofLTGT)
import           Data.Proxy                       (Proxy (Proxy))
import           Data.Tree.AVL.Intern.Constructor (AVL (EmptyAVL, ForkAVL), AlmostAVL (AlmostAVL, NullAVL))
import           Data.Tree.AVL.Invariants         (BS (Balanced, LeftHeavy, RightHeavy),
                                                   BalancedHeights, BalancedState, Height,
                                                   US (LeftUnbalanced, NotUnbalanced, RightUnbalanced),
                                                   UnbalancedState)

import           Data.Tree.BST.Invariants         (GtN, LtN)
import           Data.Tree.BST.InvariantsProofs   (proofGtNGT, proofGtNGTGtN,
                                                   proofGtNLeftSubTree,
                                                   proofGtNRightSubTree,
                                                   proofLtNLT, proofLtNLTLtN,
                                                   proofLtNLeftSubTree,
                                                   proofLtNRightSubTree)
import           Data.Tree.ITree                  (Tree (EmptyTree, ForkTree))
import           Data.Tree.Node                   (Node (Node))
import           Data.Type.Bool                   (If)
import           Data.Type.Equality               ((:~:) (Refl), gcastWith)
import           GHC.TypeLits                     (type (+), type (<=?), Nat)
import           Prelude                          (Bool (True), ($))


-- | This class provides the functionality to balance
-- | an AlmostAVL 't' (a tree that came up of an insertion or deletion
-- | on an AVL).
-- | The insertion is defined at the value level and the type level;
class Balanceable (t :: Tree) where
  type Balance (t :: Tree) :: Tree
  balance :: AlmostAVL t -> AVL (Balance t)
instance Balanceable 'EmptyTree where
  type Balance 'EmptyTree = 'EmptyTree
  balance NullAVL = EmptyAVL
instance (Balanceable' ('ForkTree l (Node n a) r) (UnbalancedState (Height l) (Height r))) =>
  Balanceable ('ForkTree l (Node n a) r) where
  type Balance ('ForkTree l (Node n a) r) = Balance' ('ForkTree l (Node n a) r) (UnbalancedState (Height l) (Height r))
  balance t@(AlmostAVL _ (Node _) _) = balance' t (Proxy::Proxy (UnbalancedState (Height l) (Height r)))

-- | This class provides the functionality to balance
-- | an AlmostAVL 't'.
-- | It's only used by the 'Balanceable' class and it has one extra parameter 'us',
-- | which is the Unbalance State of the two sub trees of 't'.
-- | The 'us' parameter guides the insertion.
class Balanceable' (t :: Tree) (us :: US) where
  type Balance' (t :: Tree) (us :: US) :: Tree
  balance' :: AlmostAVL t -> Proxy us -> AVL (Balance' t us)
instance (BalancedHeights (Height l) (Height r) ~ 'True) =>
  Balanceable' ('ForkTree l (Node n a) r) 'NotUnbalanced where
  type Balance' ('ForkTree l (Node n a) r) 'NotUnbalanced = ('ForkTree l (Node n a) r)
  balance' (AlmostAVL l node r) _ = ForkAVL l node r
instance Rotateable ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced (BalancedState (Height ll) (Height lr)) =>
  Balanceable' ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced where
  type Balance' ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced =
    Rotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced (BalancedState (Height ll) (Height lr))
  balance' t@(AlmostAVL _ (Node _) _) pus = rotate t pus (Proxy::Proxy (BalancedState (Height ll) (Height lr)))
instance Rotateable ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced (BalancedState (Height rl) (Height rr)) =>
  Balanceable' ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced where
  type Balance' ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced =
    Rotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced (BalancedState (Height rl) (Height rr))
  balance' t@(AlmostAVL _ (Node _) _) pus = rotate t pus (Proxy::Proxy (BalancedState (Height rl) (Height rr)))


-- | This class provides the functionality to apply a rotation to
-- | an AlmostAVL tree 't'.
-- | The rotation is defined at the value level and the type level.
class Rotateable (t :: Tree) (us :: US) (bs :: BS) where
  type Rotate (t :: Tree) (us :: US) (bs :: BS) :: Tree
  rotate :: AlmostAVL t -> Proxy us -> Proxy bs -> AVL (Rotate t us bs)
-- | Left-Left case (Right rotation)
instance (l ~ 'ForkTree ll (Node ln la) lr, t ~ 'ForkTree l (Node n a) r,
  BalancedHeights (Height ll) (1 + If (Height lr <=? Height r) (Height r) (Height lr)) ~ 'True,
  BalancedHeights (Height lr) (Height r) ~ 'True) =>
  Rotateable ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'LeftHeavy where
  type Rotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'LeftHeavy =
    ('ForkTree ll (Node ln la) ('ForkTree lr (Node n a) r))
  rotate (AlmostAVL (ForkAVL ll lnode lr) xnode r) _ _ =
    gcastWith (proofLtNRightSubTree pl pn Refl) $ -- LtN lr n
    gcastWith (proofLTGT pln pn
      (gcastWith (proofLtNLT pl pn Refl) Refl)) $ -- CmpNat n ln ~ 'GT
    gcastWith (proofGtNGTGtN (Proxy::Proxy r) pn pln Refl Refl) $ -- GtN r ln
    ForkAVL ll lnode (ForkAVL lr xnode r)
    where
      pn  = Proxy::Proxy n
      pl  = Proxy::Proxy l
      pln = Proxy::Proxy ln
instance (l ~ 'ForkTree ll (Node ln la) lr, t ~ 'ForkTree l (Node n a) r,
  BalancedHeights (Height ll) (1 + If (Height lr <=? Height r) (Height r) (Height lr)) ~ 'True,
  BalancedHeights (Height lr) (Height r) ~ 'True) =>
  Rotateable ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'Balanced where
  type Rotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'Balanced =
    ('ForkTree ll (Node ln la) ('ForkTree lr (Node n a) r))
  rotate (AlmostAVL (ForkAVL ll lnode lr) xnode r) _ _ =
    gcastWith (proofLtNRightSubTree pl pn Refl) $ -- LtN lr n
    gcastWith (proofLTGT pln pn
      (gcastWith (proofLtNLT pl pn Refl) Refl)) $ -- CmpNat n ln ~ 'GT
    gcastWith (proofGtNGTGtN (Proxy::Proxy r) pn pln Refl Refl) $ -- GtN r ln
    ForkAVL ll lnode (ForkAVL lr xnode r)
    where
      pn  = Proxy::Proxy n
      pl  = Proxy::Proxy l
      pln = Proxy::Proxy ln
-- | Right-Right case (Left rotation)
instance (r ~ 'ForkTree rl (Node rn ra) rr, t ~ 'ForkTree l (Node n a) r,
  BalancedHeights (1 + If (Height l <=? Height rl) (Height rl) (Height l)) (Height rr) ~ 'True,
  BalancedHeights (Height l) (Height rl) ~ 'True) =>
  Rotateable ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'RightHeavy where
  type Rotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'RightHeavy =
    ('ForkTree ('ForkTree l (Node n a) rl) (Node rn ra) rr)
  rotate (AlmostAVL l xnode (ForkAVL rl rnode rr)) _ _ =
    gcastWith (proofGtNLeftSubTree pr pn Refl) $ -- GtN rl n
    gcastWith (proofGTLT prn pn
      (gcastWith (proofGtNGT pr pn Refl) Refl)) $ -- CmpNat n rn ~ 'LT
    gcastWith (proofLtNLTLtN (Proxy::Proxy l) pn prn Refl Refl) $ -- LtN l rn
    ForkAVL (ForkAVL l xnode rl) rnode rr
    where
      pn  = Proxy::Proxy n
      pr  = Proxy::Proxy r
      prn = Proxy::Proxy rn
instance (r ~ 'ForkTree rl (Node rn ra) rr, t ~ 'ForkTree l (Node n a) r,
  BalancedHeights (1 + If (Height l <=? Height rl) (Height rl) (Height l)) (Height rr) ~ 'True,
  BalancedHeights (Height l) (Height rl) ~ 'True) =>
  Rotateable ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'Balanced where
  type Rotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'Balanced =
    ('ForkTree ('ForkTree l (Node n a) rl) (Node rn ra) rr)
  rotate (AlmostAVL l xnode (ForkAVL rl rnode rr)) _ _ =
    gcastWith (proofGtNLeftSubTree pr pn Refl) $ -- GtN rl n
    gcastWith (proofGTLT prn pn
      (gcastWith (proofGtNGT pr pn Refl) Refl)) $ -- CmpNat n rn ~ 'LT
    gcastWith (proofLtNLTLtN (Proxy::Proxy l) pn prn Refl Refl) $ -- LtN l rn
    ForkAVL (ForkAVL l xnode rl) rnode rr
    where
      pn  = Proxy::Proxy n
      pr  = Proxy::Proxy r
      prn = Proxy::Proxy rn
-- | Left-Right case (First left rotation, then right rotation)
instance (l ~ 'ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr), t ~ 'ForkTree l (Node n a) r,
  BalancedHeights (1 + If (Height ll <=? Height lrl) (Height lrl) (Height ll)) (1 + If (Height lrr <=? Height r) (Height r) (Height lrr)) ~ 'True,
  BalancedHeights (Height ll) (Height lrl) ~ 'True, BalancedHeights (Height lrr) (Height r) ~ 'True) =>
  Rotateable ('ForkTree ('ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr)) (Node n a) r) 'LeftUnbalanced 'RightHeavy where
  type Rotate ('ForkTree ('ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr)) (Node n a) r) 'LeftUnbalanced 'RightHeavy =
    ('ForkTree ('ForkTree ll (Node ln la) lrl) (Node lrn lra) ('ForkTree lrr (Node n a) r))
  rotate (AlmostAVL (ForkAVL ll lnode (ForkAVL lrl lrnode lrr)) xnode r) _ _ =
    -- Proofs for the (new) left sub tree
    gcastWith (proofGTLT plrn pln
      (gcastWith (proofGtNGT plr pln Refl) Refl)) $ -- CmpNat ln lrn ~ 'LT
    gcastWith (proofLtNLTLtN pll pln plrn Refl Refl) $  -- LtN ll lrn
    gcastWith (proofGtNLeftSubTree plr pln Refl) $ -- GtN lrl ln
    -- Proofs for the (new) right sub tree
    gcastWith (proofLTGT plrn pn
      (gcastWith (proofLtNLT plr pn
        (gcastWith (proofLtNRightSubTree pl pn Refl) Refl)) Refl)) $  -- CmpNat n lrn ~ 'GT
    gcastWith (proofGtNGTGtN pr pn plrn Refl Refl) $ -- GtN r lrn
    gcastWith (proofLtNRightSubTree plr pn
      (gcastWith (proofLtNRightSubTree pl pn Refl) Refl)) $ -- LtN lrr n
    ForkAVL (ForkAVL ll lnode lrl) lrnode (ForkAVL lrr xnode r)
    where
      pl   = Proxy::Proxy l
      pll  = Proxy::Proxy ll
      pln  = Proxy::Proxy ln
      plr  = Proxy::Proxy ('ForkTree lrl (Node lrn lra) lrr)
      plrn = Proxy::Proxy lrn
      pn   = Proxy::Proxy n
      pr   = Proxy::Proxy r
-- | Right-Left case (First right rotation, then left rotation)
instance (r ~ 'ForkTree ('ForkTree rll (Node rln rla) rlr) (Node rn ra) rr, t ~ 'ForkTree l (Node n a) r,
  BalancedHeights (1 + If (Height l <=? Height rll) (Height rll) (Height l)) (1 + If (Height rlr <=? Height rr) (Height rr) (Height rlr)) ~ 'True,
  BalancedHeights (Height l) (Height rll) ~ 'True, BalancedHeights (Height rlr) (Height rr) ~ 'True) =>
  Rotateable ('ForkTree l (Node n a) ('ForkTree ('ForkTree rll (Node rln rla) rlr) (Node rn ra) rr)) 'RightUnbalanced 'LeftHeavy where
  type Rotate ('ForkTree l (Node n a) ('ForkTree ('ForkTree rll (Node rln rla) rlr) (Node rn ra) rr)) 'RightUnbalanced 'LeftHeavy =
    ('ForkTree ('ForkTree l (Node n a) rll) (Node rln rla) ('ForkTree rlr (Node rn ra) rr))
  rotate (AlmostAVL l xnode (ForkAVL (ForkAVL rll rlnode rlr) rnode rr)) _ _ =
    -- Proofs for the (new) right sub tree
    gcastWith (proofLTGT prln prn
      (gcastWith (proofLtNLT prl prn Refl) Refl)) $ -- CmpNat rn  rln ~ 'GT
    gcastWith (proofGtNGTGtN prr prn prln Refl Refl) $  -- GtN rr rln
    gcastWith (proofLtNRightSubTree prl prn Refl) $ -- LtN rlr rn
    -- Proofs for the (new) left sub tree
    gcastWith (proofGTLT prln pn
      (gcastWith (proofGtNGT prl pn
        (gcastWith (proofGtNLeftSubTree pr pn Refl) Refl)) Refl)) $  -- CmpNat n rln ~ 'LT
    gcastWith (proofLtNLTLtN pl pn prln Refl Refl) $ -- LtN l rln
    gcastWith (proofGtNLeftSubTree prl pn
      (gcastWith (proofGtNLeftSubTree pr pn Refl) Refl)) $ -- GtN rll n
    ForkAVL (ForkAVL l xnode rll) rlnode (ForkAVL rlr rnode rr)
    where
      pr   = Proxy::Proxy r
      prr  = Proxy::Proxy rr
      prn  = Proxy::Proxy rn
      prl  = Proxy::Proxy ('ForkTree rll (Node rln rla) rlr)
      prln = Proxy::Proxy rln
      pn   = Proxy::Proxy n
      pl   = Proxy::Proxy l


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
class ProofLtNRotate (t :: Tree) (n :: Nat) (us :: US) (bs :: BS) where
  proofLtNRotate :: (LtN t n ~ 'True) =>
    Proxy t -> Proxy n -> Proxy us -> Proxy bs -> LtN (Rotate t us bs) n :~: 'True

-- | Left-Left case (Right rotation)
instance ProofLtNRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced 'LeftHeavy where
  proofLtNRotate pt pn _ _ =
    gcastWith (proofLtNLeftSubTree pl pn proof_l_LtN_n) $ -- LtN ll n
    gcastWith (proofLtNLT pl pn proof_l_LtN_n) Refl -- CmpNat ln n ~ 'LT
    where
      pl = Proxy::Proxy ('ForkTree ll (Node ln la) lr)
      proof_l_LtN_n = proofLtNLeftSubTree pt pn Refl
instance ProofLtNRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced 'Balanced where
  proofLtNRotate pt pn _ _ =
    gcastWith (proofLtNLeftSubTree pl pn proof_l_LtN_n) $ -- LtN ll n
    gcastWith (proofLtNLT pl pn proof_l_LtN_n) Refl -- CmpNat ln n ~ 'LT
    where
      pl = Proxy::Proxy ('ForkTree ll (Node ln la) lr)
      proof_l_LtN_n = proofLtNLeftSubTree pt pn Refl

-- | Right-Right case (Left rotation)
instance ProofLtNRotate ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced 'RightHeavy where
  proofLtNRotate pt pn _ _ =
    gcastWith (proofLtNRightSubTree pr pn proof_r_LtN_n) $ -- LtN rr n
    gcastWith (proofLtNLT pr pn proof_r_LtN_n) Refl -- CmpNat rn n ~ 'LT
    where
      pr = Proxy::Proxy ('ForkTree rl (Node rn ra) rr)
      proof_r_LtN_n = proofLtNRightSubTree pt pn Refl
instance ProofLtNRotate ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced 'Balanced where
  proofLtNRotate pt pn _ _ =
    gcastWith (proofLtNRightSubTree pr pn proof_r_LtN_n) $ -- LtN rr n
    gcastWith (proofLtNLT pr pn proof_r_LtN_n) Refl -- CmpNat rn n ~ 'LT
    where
      pr = Proxy::Proxy ('ForkTree rl (Node rn ra) rr)
      proof_r_LtN_n = proofLtNRightSubTree pt pn Refl

-- | Left-Right case (First left rotation, then right rotation)
instance ProofLtNRotate ('ForkTree ('ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr)) (Node n1 a) r) n 'LeftUnbalanced 'RightHeavy where
  proofLtNRotate pt pn _ _ =
    gcastWith (proofLtNLeftSubTree pl pn proof_l_LtN_n) $ -- LtN ll n
    gcastWith (proofLtNLT pl pn proof_l_LtN_n) $ -- CmpNat ln n ~ 'LT
    gcastWith (proofLtNLeftSubTree plr pn proof_lr_LtN_n) $ -- LtN lrl n
    gcastWith (proofLtNLT plr pn proof_lr_LtN_n) Refl -- CmpNat lrn n ~ 'LT
    where
      pl  = Proxy::Proxy ('ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr))
      plr = Proxy::Proxy ('ForkTree lrl (Node lrn lra) lrr)
      proof_l_LtN_n  = proofLtNLeftSubTree pt pn Refl
      proof_lr_LtN_n = proofLtNRightSubTree pl pn proof_l_LtN_n

-- | Right-Left case (First right rotation, then left rotation)
instance ProofLtNRotate ('ForkTree l (Node n1 a) ('ForkTree ('ForkTree rll (Node rln rla) rlr) (Node rn ra) rr)) n 'RightUnbalanced 'LeftHeavy where
  proofLtNRotate pt pn _ _ =
    gcastWith (proofLtNRightSubTree pr pn proof_r_LtN_n) $ -- LtN rr n
    gcastWith (proofLtNLT pr pn proof_r_LtN_n) $ -- CmpNat rn n ~ 'LT
    gcastWith (proofLtNRightSubTree prl pn proof_rl_LtN_n) $ -- LtN rlr n
    gcastWith (proofLtNLT prl pn proof_rl_LtN_n) Refl -- CmpNat rln n ~ 'LT
    where
      pr  = Proxy::Proxy ('ForkTree ('ForkTree rll (Node rln rla) rlr) (Node rn ra) rr)
      prl = Proxy::Proxy ('ForkTree rll (Node rln rla) rlr)
      proof_r_LtN_n  = proofLtNRightSubTree pt pn Refl
      proof_rl_LtN_n = proofLtNLeftSubTree pr pn proof_r_LtN_n


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
class ProofGtNRotate (t :: Tree) (n :: Nat) (us :: US) (bs :: BS) where
  proofGtNRotate :: (GtN t n ~ 'True) =>
    Proxy t -> Proxy n -> Proxy us -> Proxy bs -> GtN (Rotate t us bs) n :~: 'True

-- | Left-Left case (Right rotation)
instance ProofGtNRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced 'LeftHeavy where
  proofGtNRotate pt pn _ _ =
    gcastWith (proofGtNLeftSubTree pl pn proof_l_GtN_n) $ -- GtN ll n
    gcastWith (proofGtNGT pl pn proof_l_GtN_n) Refl -- CmpNat ln n ~ 'GT
    where
      pl = Proxy::Proxy ('ForkTree ll (Node ln la) lr)
      proof_l_GtN_n = proofGtNLeftSubTree pt pn Refl
instance ProofGtNRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced 'Balanced where
  proofGtNRotate pt pn _ _ =
    gcastWith (proofGtNLeftSubTree pl pn proof_l_GtN_n) $ -- GtN ll n
    gcastWith (proofGtNGT pl pn proof_l_GtN_n) Refl -- CmpNat ln n ~ 'GT
    where
      pl = Proxy::Proxy ('ForkTree ll (Node ln la) lr)
      proof_l_GtN_n = proofGtNLeftSubTree pt pn Refl

-- | Right-Right case (Left rotation)
instance ProofGtNRotate ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced 'RightHeavy where
  proofGtNRotate pt pn _ _ =
    gcastWith (proofGtNRightSubTree pr pn proof_r_GtN_n) $ -- GtN rr n
    gcastWith (proofGtNGT pr pn proof_r_GtN_n) Refl -- CmpNat rn n ~ 'GT
    where
      pr = Proxy::Proxy ('ForkTree rl (Node rn ra) rr)
      proof_r_GtN_n = proofGtNRightSubTree pt pn Refl
instance ProofGtNRotate ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced 'Balanced where
  proofGtNRotate pt pn _ _ =
    gcastWith (proofGtNRightSubTree pr pn proof_r_GtN_n) $ -- GtN rr n
    gcastWith (proofGtNGT pr pn proof_r_GtN_n) Refl -- CmpNat rn n ~ 'GT
    where
      pr = Proxy::Proxy ('ForkTree rl (Node rn ra) rr)
      proof_r_GtN_n = proofGtNRightSubTree pt pn Refl

-- | Left-Right case (First left rotation, then right rotation)
instance ProofGtNRotate ('ForkTree ('ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr)) (Node n1 a) r) n 'LeftUnbalanced 'RightHeavy where
  proofGtNRotate pt pn _ _ =
    gcastWith (proofGtNLeftSubTree pl pn proof_l_GtN_n) $ -- GtN ll n
    gcastWith (proofGtNGT pl pn proof_l_GtN_n) $ -- CmpNat ln n ~ 'GT
    gcastWith (proofGtNLeftSubTree plr pn proof_lr_GtN_n) $ -- GtN lrl n
    gcastWith (proofGtNGT plr pn proof_lr_GtN_n) Refl -- CmpNat lrn n ~ 'GT
    where
      pl  = Proxy::Proxy ('ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr))
      plr = Proxy::Proxy ('ForkTree lrl (Node lrn lra) lrr)
      proof_l_GtN_n  = proofGtNLeftSubTree pt pn Refl
      proof_lr_GtN_n = proofGtNRightSubTree pl pn proof_l_GtN_n

-- | Right-Left case (First right rotation, then left rotation)
instance ProofGtNRotate ('ForkTree l (Node n1 a) ('ForkTree ('ForkTree rll (Node rln rla) rlr) (Node rn ra) rr)) n 'RightUnbalanced 'LeftHeavy where
  proofGtNRotate pt pn _ _ =
    gcastWith (proofGtNRightSubTree pr pn proof_r_GtN_n) $ -- GtN rr n
    gcastWith (proofGtNGT pr pn proof_r_GtN_n) $ -- CmpNat rn n ~ 'GT
    gcastWith (proofGtNRightSubTree prl pn proof_rl_GtN_n) $ -- GtN rlr n
    gcastWith (proofGtNGT prl pn proof_rl_GtN_n) Refl -- CmpNat rln n ~ 'GT
    where
      pr  = Proxy::Proxy ('ForkTree ('ForkTree rll (Node rln rla) rlr) (Node rn ra) rr)
      prl = Proxy::Proxy ('ForkTree rll (Node rln rla) rlr)
      proof_r_GtN_n  = proofGtNRightSubTree pt pn Refl
      proof_rl_GtN_n = proofGtNLeftSubTree pr pn proof_r_GtN_n
