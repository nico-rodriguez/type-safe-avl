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

import           Data.Proxy                       (Proxy (Proxy))
import           Data.Tree.AVL.Intern.Constructor (AVL (EmptyAVL, ForkAVL), AlmostAVL (AlmostAVL, NullAVL))
import           Data.Tree.AVL.Invariants         (BS (Balanced, LeftHeavy, RightHeavy),
                                                   BalancedHeights,
                                                   BalancedState, Height,
                                                   US (LeftUnbalanced, NotUnbalanced, RightUnbalanced),
                                                   UnbalancedState)
import           Data.Tree.BST.Invariants         (GtN, LtN)
import           Data.Tree.ITree                  (Tree (EmptyTree, ForkTree))
import           Data.Tree.Node                   (Node (Node))
import           Data.Type.Bool                   (If)
import           Data.Type.Equality               ((:~:) (Refl), gcastWith)
import           GHC.TypeLits                     (type (+), type (<=?), CmpNat,
                                                   Nat)
import           Prelude                          (Bool (True),
                                                   Ordering (GT, LT))
import           Unsafe.Coerce                    (unsafeCoerce)


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
instance (CmpNat n ln ~ 'GT, GtN r ln ~ 'True, LtN lr n ~ 'True,
  BalancedHeights (Height ll) (1 + If (Height lr <=? Height r) (Height r) (Height lr)) ~ 'True,
  BalancedHeights (Height lr) (Height r) ~ 'True) =>
  Rotateable ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'LeftHeavy where
  type Rotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'LeftHeavy =
    ('ForkTree ll (Node ln la) ('ForkTree lr (Node n a) r))
  rotate (AlmostAVL (ForkAVL ll lnode lr) xnode r) _ _ = ForkAVL ll lnode (ForkAVL lr xnode r)
instance (CmpNat n ln ~ 'GT, GtN r ln ~ 'True, LtN lr n ~ 'True,
  BalancedHeights (Height ll) (1 + If (Height lr <=? Height r) (Height r) (Height lr)) ~ 'True,
  BalancedHeights (Height lr) (Height r) ~ 'True) =>
  Rotateable ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'Balanced where
  type Rotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'Balanced =
    ('ForkTree ll (Node ln la) ('ForkTree lr (Node n a) r))
  rotate (AlmostAVL (ForkAVL ll lnode lr) xnode r) _ _ = ForkAVL ll lnode (ForkAVL lr xnode r)
-- | Right-Right case (Left rotation)
instance (CmpNat n rn ~ 'LT, LtN l rn ~ 'True, GtN rl n ~ 'True,
  BalancedHeights (1 + If (Height l <=? Height rl) (Height rl) (Height l)) (Height rr) ~ 'True,
  BalancedHeights (Height l) (Height rl) ~ 'True) =>
  Rotateable ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'RightHeavy where
  type Rotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'RightHeavy =
    ('ForkTree ('ForkTree l (Node n a) rl) (Node rn ra) rr)
  rotate (AlmostAVL l xnode (ForkAVL rl rnode rr)) _ _ = ForkAVL (ForkAVL l xnode rl) rnode rr
instance (CmpNat n rn ~ 'LT, LtN l rn ~ 'True, GtN rl n ~ 'True,
  BalancedHeights (1 + If (Height l <=? Height rl) (Height rl) (Height l)) (Height rr) ~ 'True,
  BalancedHeights (Height l) (Height rl) ~ 'True) =>
  Rotateable ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'Balanced where
  type Rotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'Balanced =
    ('ForkTree ('ForkTree l (Node n a) rl) (Node rn ra) rr)
  rotate (AlmostAVL l xnode (ForkAVL rl rnode rr)) _ _ = ForkAVL (ForkAVL l xnode rl) rnode rr
-- | Left-Right case (First left rotation, then right rotation)
instance (CmpNat ln lrn ~ 'LT, LtN ll lrn ~ 'True, CmpNat n lrn ~ 'GT, GtN r lrn ~ 'True, GtN lrl ln ~ 'True, LtN lrr n ~ 'True,
  BalancedHeights (1 + If (Height ll <=? Height lrl) (Height lrl) (Height ll)) (1 + If (Height lrr <=? Height r) (Height r) (Height lrr)) ~ 'True,
  BalancedHeights (Height ll) (Height lrl) ~ 'True, BalancedHeights (Height lrr) (Height r) ~ 'True) =>
  Rotateable ('ForkTree ('ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr)) (Node n a) r) 'LeftUnbalanced 'RightHeavy where
  type Rotate ('ForkTree ('ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr)) (Node n a) r) 'LeftUnbalanced 'RightHeavy =
    ('ForkTree ('ForkTree ll (Node ln la) lrl) (Node lrn lra) ('ForkTree lrr (Node n a) r))
  rotate (AlmostAVL (ForkAVL ll lnode (ForkAVL lrl lrnode lrr)) xnode r) _ _ =
    ForkAVL (ForkAVL ll lnode lrl) lrnode (ForkAVL lrr xnode r)
-- | Right-Left case (First right rotation, then left rotation)
instance (CmpNat n rln ~ 'LT, LtN l rln ~ 'True, CmpNat rn  rln ~ 'GT, GtN rr rln ~ 'True, GtN rll n ~ 'True, LtN rlr rn ~ 'True,
  BalancedHeights (1 + If (Height l <=? Height rll) (Height rll) (Height l)) (1 + If (Height rlr <=? Height rr) (Height rr) (Height rlr)) ~ 'True,
  BalancedHeights (Height l) (Height rll) ~ 'True, BalancedHeights (Height rlr) (Height rr) ~ 'True) =>
  Rotateable ('ForkTree l (Node n a) ('ForkTree ('ForkTree rll (Node rln rla) rlr) (Node rn ra) rr)) 'RightUnbalanced 'LeftHeavy where
  type Rotate ('ForkTree l (Node n a) ('ForkTree ('ForkTree rll (Node rln rla) rlr) (Node rn ra) rr)) 'RightUnbalanced 'LeftHeavy =
    ('ForkTree ('ForkTree l (Node n a) rll) (Node rln rla) ('ForkTree rlr (Node rn ra) rr))
  rotate (AlmostAVL l xnode (ForkAVL (ForkAVL rll rlnode rlr) rnode rr)) _ _ =
    ForkAVL (ForkAVL l xnode rll) rlnode (ForkAVL rlr rnode rr)


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
class ProofLtNRotate (t :: Tree) (n :: Nat) (us::US) (bs::BS) where
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
