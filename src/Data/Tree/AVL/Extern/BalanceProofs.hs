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

import           Data.NatProofs                 (proofGTLT, proofLTGT)
import           Data.Proxy                     (Proxy (Proxy))
import           Data.Tree.AVL.Extern.Balance   (Balanceable (Balance),
                                                 Balanceable' (Balance'),
                                                 Rotateable (Rotate))
import           Data.Tree.AVL.Invariants       (BS (Balanced, LeftHeavy, RightHeavy),
                                                 BalancedState, Height, IsAVL,
                                                 US (LeftUnbalanced, NotUnbalanced, RightUnbalanced),
                                                 UnbalancedState)
import           Data.Tree.AVL.InvariantsProofs (proofIsAVLLeftSubTree,
                                                 proofIsAVLRightSubTree)
import           Data.Tree.BST.Invariants       (GtN, IsBST, LtN)
import           Data.Tree.BST.InvariantsProofs (proofGtNGT, proofGtNGTGtN,
                                                 proofGtNLeftSubTree,
                                                 proofGtNRightSubTree,
                                                 proofIsBSTGtN,
                                                 proofIsBSTLeftSubTree,
                                                 proofIsBSTLtN,
                                                 proofIsBSTRightSubTree,
                                                 proofLtNLT, proofLtNLTLtN,
                                                 proofLtNLeftSubTree,
                                                 proofLtNRightSubTree)
import           Data.Tree.ITree                (Tree (EmptyTree, ForkTree))
import           Data.Tree.Node                 (Node)
import           Data.Type.Equality             ((:~:) (Refl), gcastWith)
import           GHC.TypeNats                   (type (<=?), Nat)
import           Prelude                        (Bool (True), ($))
import           Unsafe.Coerce                  (unsafeCoerce)


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
class ProofIsBSTRotate (t :: Tree) (us :: US) (bs :: BS) where
  proofIsBSTRotate :: (IsBST t ~ 'True) =>
    Proxy t -> Proxy us -> Proxy bs -> IsBST (Rotate t us bs) :~: 'True

-- | Left-Left case (Right rotation)
instance (l ~ 'ForkTree ll (Node ln la) lr, t ~ 'ForkTree l (Node n a) r) =>
  ProofIsBSTRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'LeftHeavy where
  proofIsBSTRotate pt _ _ =
    -- Proofs for the (new) right sub tree
    gcastWith (proofIsBSTRightSubTree pt Refl) $ -- IsBST r
    gcastWith (proofIsBSTGtN pt Refl) $ -- GtN r n
    gcastWith (proofIsBSTRightSubTree pl proof_IsBST_l) $ -- IsBST lr
    gcastWith (proofIsBSTGtN pl proof_IsBST_l) $ -- GtN lr ln
    gcastWith (proofLtNRightSubTree pl pn proof_l_LtN_n) $ -- LtN lr n
    gcastWith (proofLTGT pln pn
      (gcastWith (proofLtNLT pl pn proof_l_LtN_n) Refl)) $ -- CmpNat n ln ~ 'GT
    gcastWith (proofGtNGTGtN (Proxy::Proxy r) pn pln Refl Refl) $ -- GtN r ln
    -- Proofs for the (new) left sub tree
    gcastWith (proofIsBSTLeftSubTree pl proof_IsBST_l) $ -- IsBST ll
    gcastWith (proofIsBSTLtN pl proof_IsBST_l) Refl -- LtN ll ln
    where
      pn  = Proxy::Proxy n
      pl  = Proxy::Proxy l
      pln = Proxy::Proxy ln
      proof_IsBST_l = proofIsBSTLeftSubTree pt Refl
      proof_l_LtN_n  = proofIsBSTLtN pt Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, t ~ 'ForkTree l (Node n a) r) =>
  ProofIsBSTRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'Balanced where
  proofIsBSTRotate pt _ _ =
    -- Proofs for the (new) right sub tree
    gcastWith (proofIsBSTRightSubTree pt Refl) $ -- IsBST r
    gcastWith (proofIsBSTGtN pt Refl) $ -- GtN r n
    gcastWith (proofIsBSTRightSubTree pl proof_IsBST_l) $ -- IsBST lr
    gcastWith (proofIsBSTGtN pl proof_IsBST_l) $ -- GtN lr ln
    gcastWith (proofLtNRightSubTree pl pn proof_l_LtN_n) $ -- LtN lr n
    gcastWith (proofLTGT pln pn
      (gcastWith (proofLtNLT pl pn proof_l_LtN_n) Refl)) $ -- CmpNat n ln ~ 'GT
    gcastWith (proofGtNGTGtN (Proxy::Proxy r) pn pln Refl Refl) $ -- GtN r ln
    -- Proofs for the (new) left sub tree
    gcastWith (proofIsBSTLeftSubTree pl proof_IsBST_l) $ -- IsBST ll
    gcastWith (proofIsBSTLtN pl proof_IsBST_l) Refl -- LtN ll ln
    where
      pn  = Proxy::Proxy n
      pl  = Proxy::Proxy l
      pln = Proxy::Proxy ln
      proof_IsBST_l = proofIsBSTLeftSubTree pt Refl
      proof_l_LtN_n  = proofIsBSTLtN pt Refl

-- | Right-Right case (Left rotation)
instance (r ~ 'ForkTree rl (Node rn ra) rr, t ~ 'ForkTree l (Node n a) r) =>
  ProofIsBSTRotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'RightHeavy where
  proofIsBSTRotate pt _ _ =
    -- Proofs for the (new) left sub tree
    gcastWith (proofIsBSTLeftSubTree pt Refl) $ -- IsBST l
    gcastWith (proofIsBSTLtN pt Refl) $ -- LtN l n
    gcastWith (proofIsBSTLeftSubTree pr proof_IsBST_r) $ -- IsBST rl
    gcastWith (proofIsBSTLtN pr proof_IsBST_r) $ -- LtN rl rn
    gcastWith (proofGtNLeftSubTree pr pn proof_GtN_r) $ -- GtN rl n
    gcastWith (proofGTLT prn pn
      (gcastWith (proofGtNGT pr pn proof_GtN_r) Refl)) $ -- CmpNat n rn ~ 'LT
    gcastWith (proofLtNLTLtN (Proxy::Proxy l) pn prn Refl Refl) $ -- LtN l rn
    -- Proofs for the (new) right sub tree
    gcastWith (proofIsBSTRightSubTree pr proof_IsBST_r) $ -- IsBST rr
    gcastWith (proofIsBSTGtN pr proof_IsBST_r) Refl -- GtN rr rn
    where
      pn  = Proxy::Proxy n
      pr  = Proxy::Proxy r
      prn = Proxy::Proxy rn
      proof_IsBST_r = proofIsBSTRightSubTree pt Refl
      proof_GtN_r = proofIsBSTGtN pt Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, t ~ 'ForkTree l (Node n a) r) =>
  ProofIsBSTRotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'Balanced where
  proofIsBSTRotate pt _ _ =
    -- Proofs for the (new) left sub tree
    gcastWith (proofIsBSTLeftSubTree pt Refl) $ -- IsBST l
    gcastWith (proofIsBSTLtN pt Refl) $ -- LtN l n
    gcastWith (proofIsBSTLeftSubTree pr proof_IsBST_r) $ -- IsBST rl
    gcastWith (proofIsBSTLtN pr proof_IsBST_r) $ -- LtN rl rn
    gcastWith (proofGtNLeftSubTree pr pn proof_GtN_r) $ -- GtN rl n
    gcastWith (proofGTLT prn pn
      (gcastWith (proofGtNGT pr pn proof_GtN_r) Refl)) $ -- CmpNat n rn ~ 'LT
    gcastWith (proofLtNLTLtN (Proxy::Proxy l) pn prn Refl Refl) $ -- LtN l rn
    -- Proofs for the (new) right sub tree
    gcastWith (proofIsBSTRightSubTree pr proof_IsBST_r) $ -- IsBST rr
    gcastWith (proofIsBSTGtN pr proof_IsBST_r) Refl -- GtN rr rn
    where
      pn  = Proxy::Proxy n
      pr  = Proxy::Proxy r
      prn = Proxy::Proxy rn
      proof_IsBST_r = proofIsBSTRightSubTree pt Refl
      proof_GtN_r = proofIsBSTGtN pt Refl

-- | Left-Right case (First left rotation, then right rotation)
instance (lr ~ 'ForkTree lrl (Node lrn lra) lrr, l ~ 'ForkTree ll (Node ln la) lr, t ~ 'ForkTree l (Node n a) r) =>
  ProofIsBSTRotate ('ForkTree ('ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr)) (Node n a) r) 'LeftUnbalanced 'RightHeavy where
  proofIsBSTRotate pt _ _ =
    -- Proofs for the (new) right sub tree
    gcastWith (proofIsBSTRightSubTree pt Refl) $ -- IsBST r
    gcastWith (proofIsBSTGtN pt Refl) $ -- GtN r n
    gcastWith (proofIsBSTRightSubTree plr proof_IsBST_lr) $ -- IsBST lrr
    gcastWith (proofIsBSTGtN plr proof_IsBST_lr) $ -- GtN lrr lrn
    gcastWith (proofLtNRightSubTree plr pn proof_lr_LtN_n) $ -- LtN lrr n
    gcastWith (proofLTGT plrn pn
      (gcastWith (proofLtNLT plr pn proof_lr_LtN_n) Refl)) $ -- CmpNat n lrn ~ 'GT
    gcastWith (proofGtNGTGtN (Proxy::Proxy r) pn plrn Refl Refl) $ -- GtN r lrn
    -- Proofs for the (new) left sub tree
    gcastWith (proofIsBSTLeftSubTree pl proof_IsBST_l) $ -- IsBST ll
    gcastWith (proofIsBSTLtN pl proof_IsBST_l) $ -- LtN ll ln
    gcastWith (proofIsBSTLeftSubTree plr proof_IsBST_lr) $ -- IsBST lrl
    gcastWith (proofIsBSTLtN plr proof_IsBST_lr) $ -- LtN lrl lrn
    gcastWith (proofGtNLeftSubTree plr pln
      (gcastWith (proofIsBSTGtN pl proof_IsBST_l) Refl)) $ -- GtN lrl ln
    gcastWith (proofGTLT plrn pln
      (gcastWith (proofGtNGT plr pln
      (gcastWith (proofIsBSTGtN pl proof_IsBST_l) Refl)) Refl)) $ -- CmpNat ln lrn ~ 'LT
    gcastWith (proofLtNLTLtN pll pln plrn Refl Refl) Refl -- LtN ll lrn
    where
      pn   = Proxy::Proxy n
      pl   = Proxy::Proxy l
      pln  = Proxy::Proxy ln
      pll  = Proxy::Proxy ll
      plrn = Proxy::Proxy lrn
      plr  = Proxy::Proxy lr
      proof_IsBST_l  = proofIsBSTLeftSubTree pt Refl
      proof_IsBST_lr = proofIsBSTRightSubTree pl proof_IsBST_l
      proof_lr_LtN_n = proofLtNRightSubTree pl pn (proofIsBSTLtN pt Refl)

-- | Right-Left case (First right rotation, then left rotation)
instance (rl ~ 'ForkTree rll (Node rln rla) rlr, r ~ 'ForkTree rl (Node rn ra) rr, t ~ 'ForkTree l (Node n a) r) =>
  ProofIsBSTRotate ('ForkTree l (Node n a) ('ForkTree ('ForkTree rll (Node rln rla) rlr) (Node rn ra) rr)) 'RightUnbalanced 'LeftHeavy where
  proofIsBSTRotate pt _ _ =
    -- Proofs for the (new) left sub tree
    gcastWith (proofIsBSTLeftSubTree pt Refl) $ -- IsBST l
    gcastWith (proofIsBSTLtN pt Refl) $ -- LtN l n
    gcastWith (proofIsBSTLeftSubTree prl proof_IsBST_rl) $ -- IsBST rll
    gcastWith (proofIsBSTLtN prl proof_IsBST_rl) $ -- LtN rll rln
    gcastWith (proofGtNLeftSubTree prl pn proof_rl_GtN_n) $ -- GtN rll n
    gcastWith (proofGTLT prln pn
      (gcastWith (proofGtNGT prl pn proof_rl_GtN_n) Refl)) $ -- CmpNat n rln ~ 'LT
    gcastWith (proofLtNLTLtN (Proxy::Proxy l) pn prln Refl Refl) $ -- LtN l rln
    -- Proofs for the (new) right sub tree
    gcastWith (proofIsBSTRightSubTree pr proof_IsBST_r) $ -- IsBST rr
    gcastWith (proofIsBSTGtN pr proof_IsBST_r) $ -- GtN rr rn
    gcastWith (proofIsBSTRightSubTree prl proof_IsBST_rl) $ -- IsBST rlr
    gcastWith (proofIsBSTGtN prl proof_IsBST_rl) $ -- GtN rlr rln
    gcastWith (proofLtNRightSubTree prl prn
      (gcastWith (proofIsBSTLtN pr proof_IsBST_r) Refl)) $ -- LtN rlr rn
    gcastWith (proofLTGT prln prn
      (gcastWith (proofLtNLT prl prn
      (gcastWith (proofIsBSTLtN pr proof_IsBST_r) Refl)) Refl)) $ -- CmpNat rn rln ~ 'GT
    gcastWith (proofGtNGTGtN prr prn prln Refl Refl) Refl -- GtN rr rln
    where
      pn   = Proxy::Proxy n
      pr   = Proxy::Proxy r
      prn  = Proxy::Proxy rn
      prl  = Proxy::Proxy rl
      prln = Proxy::Proxy rln
      prr  = Proxy::Proxy rr
      proof_IsBST_r  = proofIsBSTRightSubTree pt Refl
      proof_IsBST_rl = proofIsBSTLeftSubTree pr proof_IsBST_r
      proof_rl_GtN_n = proofGtNLeftSubTree pr pn (proofIsBSTGtN pt Refl)


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
class ProofIsAVLBalance' (t :: Tree) (us :: US) where
  proofIsAVLBalance' :: (t ~ 'ForkTree l (Node n a) r, IsAVL l ~ 'True, IsAVL r ~ 'True, LtN l n ~ 'True, GtN r n ~ 'True) =>
    Proxy t -> Proxy us -> IsAVL (Balance' t us) :~: 'True
-- | NotUnbalanced implies BalancedHeights (Height l) (Height r) ~ 'True
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
-- | IsAVL l ~ 'True && IsAVL r ~ 'True && LtN l n ~ 'True && GtN r n ~ 'True.
class ProofIsAVLRotate (t :: Tree) (us :: US) (bs :: BS) where
  proofIsAVLRotate :: (t ~ 'ForkTree l (Node n a) r, IsAVL l ~ 'True, IsAVL r ~ 'True, LtN l n ~ 'True, GtN r n ~ 'True) =>
    Proxy t -> Proxy us -> Proxy bs -> IsAVL (Rotate t us bs) :~: 'True
-- | Left-Left case (Right rotation)
instance ((Height lr <=? Height r) ~ 'True) =>
  ProofIsAVLRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'LeftHeavy where
  proofIsAVLRotate _ _ _ =
    gcastWith (proofIsAVLLeftSubTree pl Refl) $ -- IsAVL ll
    gcastWith (proofIsAVLRightSubTree pl Refl) $ -- IsAVL lr
    -- BalancedHeights (Height lr) (Height r)
    -- BalancedHeights (Height ll) (1 + (Height r))
    unsafeCoerce Refl
    where
      pl = Proxy::Proxy ('ForkTree ll (Node ln la) lr)
instance ((Height lr <=? Height r) ~ 'True) =>
  ProofIsAVLRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'Balanced where
  proofIsAVLRotate _ _ _ =
    gcastWith (proofIsAVLLeftSubTree pl Refl) $ -- IsAVL ll
    gcastWith (proofIsAVLRightSubTree pl Refl) $ -- IsAVL lr
    -- BalancedHeights (Height lr) (Height r) ~ 'True
    -- BalancedHeights (Height ll) (1 + (Height r))
    unsafeCoerce Refl
    where
      pl = Proxy::Proxy ('ForkTree ll (Node ln la) lr)

-- | Right-Right case (Left rotation)
instance ((Height l <=? Height rl) ~ 'True) =>
  ProofIsAVLRotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'RightHeavy where
  proofIsAVLRotate _ _ _ =
    gcastWith (proofIsAVLLeftSubTree pr Refl) $ -- IsAVL rl
    gcastWith (proofIsAVLRightSubTree pr Refl) $ -- IsAVL rr
    -- BalancedHeights (Height l) (Height rl)
    -- BalancedHeights (1 + (Height rl)) (Height rr)
    unsafeCoerce Refl
    where
      pr = Proxy::Proxy ('ForkTree rl (Node rn ra) rr)
instance ((Height l <=? Height rl) ~ 'True) =>
  ProofIsAVLRotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'Balanced where
  proofIsAVLRotate _ _ _ =
    gcastWith (proofIsAVLLeftSubTree pr Refl) $ -- IsAVL rl
    gcastWith (proofIsAVLRightSubTree pr Refl) $ -- IsAVL rr
    -- BalancedHeights (Height l) (Height rl)
    -- BalancedHeights (1 + (Height rl)) (Height rr)
    unsafeCoerce Refl
    where
      pr = Proxy::Proxy ('ForkTree rl (Node rn ra) rr)

-- | Left-Right case (First left rotation, then right rotation)
instance ((Height ll <=? Height lrl) ~ 'True, (Height lrr <=? Height r) ~ 'True) =>
  ProofIsAVLRotate ('ForkTree ('ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr)) (Node n a) r) 'LeftUnbalanced 'RightHeavy where
  proofIsAVLRotate _ _ _ =
    gcastWith (proofIsAVLLeftSubTree pl Refl) $ -- IsAVL ll
    gcastWith (proofIsAVLLeftSubTree plr proof_IsAVL_lr) $ -- IsAVL lrl
    gcastWith (proofIsAVLRightSubTree plr proof_IsAVL_lr) $ -- IsAVL lrr
    -- BalancedHeights (Height ll) (Height lrl)
    -- BalancedHeights (Height lrr) (Height r)
    -- BalancedHeights (1 + If (Height ll <=? Height lrl) (Height lrl) (Height ll)) (1 + If (Height lrr <=? Height r) (Height r) (Height lrr))
    unsafeCoerce Refl
    where
      pl  = Proxy::Proxy ('ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr))
      plr = Proxy::Proxy ('ForkTree lrl (Node lrn lra) lrr)
      proof_IsAVL_lr = proofIsAVLRightSubTree pl Refl

-- | Right-Left case (First right rotation, then left rotation)
instance ((Height l <=? Height rll) ~ 'True, (Height rlr <=? Height rr) ~ 'True) =>
  ProofIsAVLRotate ('ForkTree l (Node n a) ('ForkTree ('ForkTree rll (Node rln rla) rlr) (Node rn ra) rr)) 'RightUnbalanced 'LeftHeavy where
  proofIsAVLRotate _ _ _ =
    gcastWith (proofIsAVLRightSubTree pr Refl) $ -- IsAVL rr
    gcastWith (proofIsAVLLeftSubTree prl proof_IsAVL_rl) $ -- IsAVL rll
    gcastWith (proofIsAVLRightSubTree prl proof_IsAVL_rl) $ -- IsAVL rlr
    -- BalancedHeights (Height l) (Height rll)
    -- BalancedHeights (Height rlr) (Height rr)
    -- BalancedHeights (1 + (Height rll)) (1 + (Height rr))
    unsafeCoerce Refl
    where
      pr  = Proxy::Proxy ('ForkTree ('ForkTree rll (Node rln rla) rlr) (Node rn ra) rr)
      prl = Proxy::Proxy ('ForkTree rll (Node rln rla) rlr)
      proof_IsAVL_rl = proofIsAVLLeftSubTree pr Refl
