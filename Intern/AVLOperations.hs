{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Intern.AVLOperations where

import           Data.Kind
import           Data.Proxy
import           Data.Type.Bool
import           Data.Type.Equality
import           GHC.TypeLits
import           ITree
import           Node

-- | Check if all elements of the tree are strictly less than x
type family LtN (l :: Tree) (x :: Nat) :: Bool where
  LtN 'EmptyTree        x = 'True
  LtN ('ForkTree l (Node n a) r) x = CmpNat n x == 'LT && LtN l x && LtN r x

-- | Check if all elements of the tree are strictly greater than x
type family GtN (r :: Tree) (x :: Nat) :: Bool where
  GtN 'EmptyTree        x = 'True
  GtN ('ForkTree l (Node n a) r) x = CmpNat n x == 'GT && GtN l x && GtN r x

type family Max (n1 :: Nat) (n2 :: Nat) :: Nat where
  Max n1 n2 =
    (If (n1 <=? n2)
      n2
      n1
    )

type family Height (t :: Tree) :: Nat where
  Height 'EmptyTree = 0
  Height ('ForkTree l (Node n a) r) = 1 + Max (Height l) (Height r)

type family BalancedHeights (h1 :: Nat) (h2 :: Nat) :: Bool where
  BalancedHeights 0  0  = 'True
  BalancedHeights 1  0  = 'True
  BalancedHeights h1 0  = 'False
  BalancedHeights 0  1  = 'True
  BalancedHeights 0  h2 = 'False
  BalancedHeights h1 h2 = BalancedHeights (h1-1) (h2-1)

data AVL :: Tree -> Type where
  EmptyAVL :: AVL 'EmptyTree
  ForkAVL  :: (Show a, LtN l n ~ 'True, GtN r n ~ 'True, BalancedHeights (Height l) (Height r) ~ 'True) =>
    AVL l -> Node n a -> AVL r -> AVL ('ForkTree l (Node n a) r)

instance Show (AVL t) where
  show EmptyAVL         = "E"
  show (ForkAVL l n@(Node _) r)  = "F " ++ go l ++ " " ++ show n ++ " " ++ go r
    where
      go :: AVL t' -> String
      go EmptyAVL         = "E"
      go (ForkAVL l' n'@(Node _) r')  = "(F " ++ go l' ++ " " ++ show n' ++ " " ++ go r' ++ ")"

data AlmostAVL :: Tree -> Type where
  NullAVL    :: AlmostAVL 'EmptyTree
  AlmostAVL  :: (Show a, LtN l n ~ 'True, GtN r n ~ 'True) =>
    AVL l -> Node n a -> AVL r -> AlmostAVL ('ForkTree l (Node n a) r)

data US = LeftUnbalanced | RightUnbalanced | NotUnbalanced

type family UnbalancedState (h1 :: Nat) (h2 :: Nat) :: US where
  UnbalancedState 0 0   = 'NotUnbalanced
  UnbalancedState 1 0   = 'NotUnbalanced
  UnbalancedState 0 1   = 'NotUnbalanced
  UnbalancedState 2 0   = 'LeftUnbalanced
  UnbalancedState 0 2   = 'RightUnbalanced
  UnbalancedState h1 h2 = UnbalancedState (h1-1) (h2-1)

data BS = LeftHeavy | RightHeavy | Balanced

type family BalancedState (h1 :: Nat) (h2 :: Nat) :: BS where
  BalancedState 0 0   = 'Balanced
  BalancedState 1 0   = 'LeftHeavy
  BalancedState 0 1   = 'RightHeavy
  BalancedState h1 h2 = BalancedState (h1-1) (h2-1)

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

class ProofLtNInsert' (x :: Nat) (a :: Type) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofLtNInsert' :: (t ~ 'ForkTree l n1 r, CmpNat x n ~ 'LT, LtN t n ~ 'True) =>
    Node x a -> AVL t -> Proxy n -> Proxy o -> LtN (Insert x a t) n :~: 'True
instance (t ~ 'ForkTree l n1 r, CmpNat x n1 ~ 'EQ, CmpNat x n ~ 'LT, CmpNat n1 n ~ 'LT, LtN l n ~ 'True, LtN r n ~ 'True) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) r) n 'EQ where
  proofLtNInsert' _ ForkAVL{} _ _ = Refl
instance (t ~ 'ForkTree l n1 r, l ~ 'EmptyTree, CmpNat x n1 ~ 'LT, CmpNat x n ~ 'LT, CmpNat n1 n ~ 'LT, LtN l n ~ 'True, LtN r n ~ 'True,
  ProofLtNBalance ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n1 a1) r) n) =>
  ProofLtNInsert' x a ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofLtNInsert' _ (ForkAVL EmptyAVL _ _) _ _ =
    gcastWith (proofLtNBalance (Proxy::Proxy ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n1 a1) r)) (Proxy::Proxy n)) Refl
instance (t ~ 'ForkTree l (Node n1 a1) r, l ~ 'ForkTree ll (Node ln lna) lr, CmpNat x n1 ~ 'LT, CmpNat x n ~ 'LT, CmpNat n1 n ~ 'LT, LtN l n ~ 'True, LtN r n ~ 'True,
  ProofLtNInsert' x a l n (CmpNat x ln), ProofLtNBalance ('ForkTree (Insert' x a ('ForkTree ll (Node ln lna) lr) (CmpNat x ln)) (Node n1 a1) r) n) =>
  ProofLtNInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n1 a1) r) n 'LT where
  proofLtNInsert' node (ForkAVL l@ForkAVL{} _ _) n _ =
    gcastWith (proofLtNInsert' node l n (Proxy::Proxy (CmpNat x ln))) $
      gcastWith (proofLtNBalance (Proxy::Proxy ('ForkTree (Insert' x a ('ForkTree ll (Node ln lna) lr) (CmpNat x ln)) (Node n1 a1) r)) (Proxy::Proxy n)) Refl
instance (t ~ 'ForkTree l (Node n1 a1) r, r ~ 'EmptyTree, CmpNat x n1 ~ 'GT, CmpNat x n ~ 'LT, CmpNat n1 n ~ 'LT, LtN l n ~ 'True, LtN r n ~ 'True,
  ProofLtNBalance ('ForkTree l (Node n1 a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree)) n) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofLtNInsert' _ (ForkAVL _ _ EmptyAVL) _ _ =
    gcastWith (proofLtNBalance (Proxy::Proxy ('ForkTree l (Node n1 a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree))) (Proxy::Proxy n)) Refl
instance (t ~ 'ForkTree l (Node n1 a1) r, r ~ 'ForkTree rl (Node rn rna) rr, CmpNat x n1 ~ 'GT, CmpNat x n ~ 'LT, CmpNat n1 n ~ 'LT, LtN l n ~ 'True, LtN r n ~ 'True,
  ProofLtNInsert' x a r n (CmpNat x rn),
  ProofLtNBalance ('ForkTree l (Node n1 a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn))) n) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn rna) rr)) n 'GT where
  proofLtNInsert' node (ForkAVL _ _ r@ForkAVL{}) n _ =
    gcastWith (proofLtNInsert' node r n (Proxy::Proxy (CmpNat x rn))) $
      gcastWith (proofLtNBalance (Proxy::Proxy ('ForkTree l (Node n1 a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn)))) (Proxy::Proxy n)) Refl

class ProofGtNInsert' (x :: Nat) (a :: Type) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofGtNInsert' :: (t ~ 'ForkTree l n1 r, CmpNat x n ~ 'GT, GtN t n ~ 'True) =>
    Node x a -> AVL t -> Proxy n -> Proxy o -> GtN (Insert x a t) n :~: 'True
instance (t ~ 'ForkTree l (Node n1 a1) r, CmpNat x n1 ~ 'EQ, CmpNat x n ~ 'GT, CmpNat n1 n ~ 'GT, GtN l n ~ 'True, GtN r n ~ 'True) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) r) n 'EQ where
  proofGtNInsert' _ ForkAVL{} _ _ = Refl
instance (t ~ 'ForkTree l (Node n1 a1) r, l ~ 'EmptyTree, CmpNat x n1 ~ 'LT, CmpNat x n ~ 'GT, CmpNat n1 n ~ 'GT, GtN l n ~ 'True, GtN r n ~ 'True,
  ProofGtNBalance ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n1 a1) r) n) =>
  ProofGtNInsert' x a ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofGtNInsert' _ (ForkAVL EmptyAVL _ _) _ _ =
    gcastWith (proofGtNBalance (Proxy::Proxy ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n1 a1) r)) (Proxy::Proxy n)) Refl
instance (t ~ 'ForkTree l (Node n1 a1) r, l ~ 'ForkTree ll (Node ln lna) lr, CmpNat x n1 ~ 'LT, CmpNat x n ~ 'GT, CmpNat n1 n ~ 'GT, GtN l n ~ 'True, GtN r n ~ 'True,
  ProofGtNInsert' x a l n (CmpNat x ln),
  ProofGtNBalance ('ForkTree (Insert' x a ('ForkTree ll (Node ln lna) lr) (CmpNat x ln)) (Node n1 a1) r) n) =>
  ProofGtNInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n1 a1) r) n 'LT where
  proofGtNInsert' x (ForkAVL l@ForkAVL{} _ _) n _ =
    gcastWith (proofGtNInsert' x l n (Proxy::Proxy (CmpNat x ln))) $
      gcastWith (proofGtNBalance (Proxy::Proxy ('ForkTree (Insert' x a ('ForkTree ll (Node ln lna) lr) (CmpNat x ln)) (Node n1 a1) r)) (Proxy::Proxy n)) Refl
instance (t ~ 'ForkTree l (Node n1 a1) r, r ~ 'EmptyTree, CmpNat x n1 ~ 'GT, CmpNat x n ~ 'GT, CmpNat n1 n ~ 'GT, GtN l n ~ 'True, GtN r n ~ 'True,
  ProofGtNBalance ('ForkTree l (Node n1 a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree)) n) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofGtNInsert' _ (ForkAVL _ _ EmptyAVL) _ _ =
    gcastWith (proofGtNBalance (Proxy::Proxy ('ForkTree l (Node n1 a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree))) (Proxy::Proxy n)) Refl
instance (t ~ 'ForkTree l (Node n1 a1) r, r ~ 'ForkTree rl (Node rn rna) rr, CmpNat x n1 ~ 'GT, CmpNat x n ~ 'GT, CmpNat n1 n ~ 'GT, GtN l n ~ 'True, GtN r n ~ 'True,
  ProofGtNInsert' x a r n (CmpNat x rn),
  ProofGtNBalance ('ForkTree l (Node n1 a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn))) n) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn rna) rr)) n 'GT where
  proofGtNInsert' x (ForkAVL _ _ r@ForkAVL{}) n _ =
    gcastWith (proofGtNInsert' x r n (Proxy::Proxy (CmpNat x rn))) $
      gcastWith (proofGtNBalance (Proxy::Proxy ('ForkTree l (Node n1 a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn)))) (Proxy::Proxy n)) Refl

class ProofLtNBalance (t :: Tree) (n :: Nat) where
  proofLtNBalance :: (LtN t n ~ 'True) =>
    Proxy t -> Proxy n -> LtN (Balance t) n :~: 'True
instance ProofLtNBalance 'EmptyTree n where
  proofLtNBalance _ _ = Refl
instance (ProofLtNBalance' ('ForkTree l (Node n1 a) r) n (UnbalancedState (Height l) (Height r))) =>
  ProofLtNBalance ('ForkTree l (Node n1 a) r) n where
  proofLtNBalance pt pn = gcastWith (proofLtNBalance' pt pn (Proxy::Proxy (UnbalancedState (Height l) (Height r)))) Refl

class ProofLtNBalance' (t :: Tree) (n :: Nat) (us :: US) where
  proofLtNBalance' :: Proxy t -> Proxy n -> Proxy us -> LtN (Balance' t us) n :~: 'True
instance (LtN ('ForkTree l (Node n1 a) r) n ~ 'True) =>
  ProofLtNBalance' ('ForkTree l (Node n1 a) r) n 'NotUnbalanced where
  proofLtNBalance' _ _ _ = Refl
instance (ProofLtNRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced (BalancedState (Height ll) (Height lr))) =>
  ProofLtNBalance' ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced where
  proofLtNBalance' pt pn pus = gcastWith (proofLtNRotate pt pn pus (Proxy::Proxy (BalancedState (Height ll) (Height lr)))) Refl
instance ProofLtNRotate ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced (BalancedState (Height rl) (Height rr)) =>
  ProofLtNBalance' ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced where
  proofLtNBalance' pt pn pus = gcastWith (proofLtNRotate pt pn pus (Proxy::Proxy (BalancedState (Height rl) (Height rr)))) Refl

class ProofLtNRotate (t :: Tree) (n :: Nat) (us::US) (bs::BS) where
  proofLtNRotate :: Proxy t -> Proxy n -> Proxy us -> Proxy bs -> LtN (Rotate t us bs) n :~: 'True
-- | Left-Left case (Right rotation)
instance (LtN lr n ~ 'True, GtN r n ~ 'True, LtN ll ln ~ 'True, CmpNat n ln ~ 'GT,
  GtN lr ln ~ 'True, GtN r ln ~ 'True, CmpNat ln n ~ 'LT, LtN ll n ~ 'True, CmpNat n1 n ~ 'LT, LtN r n ~ 'True) =>
  ProofLtNRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced 'LeftHeavy where
  proofLtNRotate _ _ _ _ = Refl
instance (LtN lr n ~ 'True, GtN r n ~ 'True, LtN ll ln ~ 'True, CmpNat n ln ~ 'GT,
  GtN lr ln ~ 'True, GtN r ln ~ 'True, CmpNat ln n ~ 'LT, LtN ll n ~ 'True, CmpNat n1 n ~ 'LT, LtN r n ~ 'True) =>
  ProofLtNRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced 'Balanced where
  proofLtNRotate _ _ _ _ = Refl
-- | Right-Right case (Left rotation)
instance (LtN l n ~ 'True, GtN rl n ~ 'True, CmpNat n rn ~ 'LT, LtN l rn ~ 'True,
  LtN rl rn ~ 'True, GtN rr rn ~ 'True, CmpNat rn n ~ 'LT, CmpNat n1 n ~ 'LT, LtN rl n ~ 'True, LtN rr n ~ 'True) =>
  ProofLtNRotate ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced 'RightHeavy where
  proofLtNRotate _ _ _ _ = Refl
instance (LtN l n ~ 'True, GtN rl n ~ 'True, CmpNat n rn ~ 'LT, LtN l rn ~ 'True,
  LtN rl rn ~ 'True, GtN rr rn ~ 'True, CmpNat rn n ~ 'LT, CmpNat n1 n ~ 'LT, LtN rl n ~ 'True, LtN rr n ~ 'True) =>
  ProofLtNRotate ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced 'Balanced where
  proofLtNRotate _ _ _ _ = Refl
-- | Left-Right case (First left rotation, then right rotation)
instance (LtN ll ln ~ 'True, GtN lrl ln ~ 'True, LtN lrr n ~ 'True,
  GtN r n ~ 'True, CmpNat ln lrn ~ 'LT, LtN ll lrn ~ 'True, LtN lrl lrn ~ 'True, CmpNat n lrn ~ 'GT, GtN lrr lrn ~ 'True, GtN r lrn ~ 'True,
  CmpNat lrn n ~ 'LT, CmpNat ln n ~ 'LT, LtN ll n ~ 'True, LtN lrl n ~ 'True, CmpNat n1 n ~ 'LT, LtN r n ~ 'True) =>
  ProofLtNRotate ('ForkTree ('ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr)) (Node n1 a) r) n 'LeftUnbalanced 'RightHeavy where
  proofLtNRotate _ _ _ _ = Refl
-- | Right-Left case (First right rotation, then left rotation)
instance (LtN l n ~ 'True, GtN rll n ~ 'True, LtN rlr rn ~ 'True,
  GtN rr rn ~ 'True, CmpNat n rln ~ 'LT, LtN l rln ~ 'True, LtN rll rln ~ 'True, CmpNat rn rln ~ 'GT, GtN rlr rln ~ 'True, GtN rr rln ~ 'True,
  CmpNat rln n ~ 'LT, CmpNat n1 n ~ 'LT, LtN rll n ~ 'True, CmpNat rn n ~ 'LT, LtN rll n ~ 'True, LtN rr n ~ 'True, LtN rlr n ~ 'True) =>
  ProofLtNRotate ('ForkTree l (Node n1 a) ('ForkTree ('ForkTree rll (Node rln rla) rlr) (Node rn ra) rr)) n 'RightUnbalanced 'LeftHeavy where
  proofLtNRotate _ _ _ _ = Refl

class ProofGtNBalance (t :: Tree) (n :: Nat) where
  proofGtNBalance :: (GtN t n ~ 'True) =>
    Proxy t -> Proxy n -> GtN (Balance t) n :~: 'True
instance ProofGtNBalance 'EmptyTree n where
  proofGtNBalance _ _ = Refl
instance (ProofGtNBalance' ('ForkTree l (Node n1 a) r) n (UnbalancedState (Height l) (Height r))) =>
  ProofGtNBalance ('ForkTree l (Node n1 a) r) n where
  proofGtNBalance pt pn = gcastWith (proofGtNBalance' pt pn (Proxy::Proxy (UnbalancedState (Height l) (Height r)))) Refl

class ProofGtNBalance' (t :: Tree) (n :: Nat) (us :: US) where
  proofGtNBalance' :: Proxy t -> Proxy n -> Proxy us -> GtN (Balance' t us) n :~: 'True
instance (GtN ('ForkTree l (Node n1 a) r) n ~ 'True) =>
  ProofGtNBalance' ('ForkTree l (Node n1 a) r) n 'NotUnbalanced where
  proofGtNBalance' _ _ _ = Refl
instance (ProofGtNRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced (BalancedState (Height ll) (Height lr))) =>
  ProofGtNBalance' ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced where
  proofGtNBalance' pt pn pus = gcastWith (proofGtNRotate pt pn pus (Proxy::Proxy (BalancedState (Height ll) (Height lr)))) Refl
instance ProofGtNRotate ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced (BalancedState (Height rl) (Height rr)) =>
  ProofGtNBalance' ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced where
  proofGtNBalance' pt pn pus = gcastWith (proofGtNRotate pt pn pus (Proxy::Proxy (BalancedState (Height rl) (Height rr)))) Refl

class ProofGtNRotate (t :: Tree) (n :: Nat) (us::US) (bs::BS) where
  proofGtNRotate :: Proxy t -> Proxy n -> Proxy us -> Proxy bs -> GtN (Rotate t us bs) n :~: 'True
-- | Left-Left case (Right rotation)
instance (LtN lr n ~ 'True, GtN r n ~ 'True, LtN ll ln ~ 'True, CmpNat n ln ~ 'GT,
  GtN lr ln ~ 'True, GtN r ln ~ 'True, CmpNat ln n ~ 'GT, GtN ll n ~ 'True, CmpNat n1 n ~ 'GT, GtN lr n ~ 'True) =>
  ProofGtNRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced 'LeftHeavy where
  proofGtNRotate _ _ _ _ = Refl
instance (LtN lr n ~ 'True, GtN r n ~ 'True, LtN ll ln ~ 'True, CmpNat n ln ~ 'GT,
  GtN lr ln ~ 'True, GtN r ln ~ 'True, CmpNat ln n ~ 'GT, GtN ll n ~ 'True, CmpNat n1 n ~ 'GT, GtN r n ~ 'True, GtN lr n ~ 'True) =>
  ProofGtNRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced 'Balanced where
  proofGtNRotate _ _ _ _ = Refl
-- | Right-Right case (Left rotation)
instance (LtN l n ~ 'True, GtN rl n ~ 'True, CmpNat n rn ~ 'LT, LtN l rn ~ 'True,
  LtN rl rn ~ 'True, GtN rr rn ~ 'True, CmpNat rn n ~ 'GT, CmpNat n1 n ~ 'GT, GtN rl n ~ 'True, GtN rr n ~ 'True, GtN l n ~ 'True) =>
  ProofGtNRotate ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced 'RightHeavy where
  proofGtNRotate _ _ _ _ = Refl
instance (LtN l n ~ 'True, GtN rl n ~ 'True, CmpNat n rn ~ 'LT, LtN l rn ~ 'True,
  LtN rl rn ~ 'True, GtN rr rn ~ 'True, CmpNat rn n ~ 'GT, CmpNat n1 n ~ 'GT, GtN rl n ~ 'True, GtN rr n ~ 'True, GtN l n ~ 'True) =>
  ProofGtNRotate ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced 'Balanced where
  proofGtNRotate _ _ _ _ = Refl
-- | Left-Right case (First left rotation, then right rotation)
instance (LtN ll ln ~ 'True, GtN lrl ln ~ 'True, LtN lrr n ~ 'True,
  GtN r n ~ 'True, CmpNat ln lrn ~ 'LT, LtN ll lrn ~ 'True, LtN lrl lrn ~ 'True, CmpNat n lrn ~ 'GT, GtN lrr lrn ~ 'True, GtN r lrn ~ 'True,
  CmpNat lrn n ~ 'GT, CmpNat ln n ~ 'GT, GtN ll n ~ 'True, GtN lrl n ~ 'True, CmpNat n1 n ~ 'GT, GtN r n ~ 'True, GtN lrr n ~ 'True) =>
  ProofGtNRotate ('ForkTree ('ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr)) (Node n1 a) r) n 'LeftUnbalanced 'RightHeavy where
  proofGtNRotate _ _ _ _ = Refl
-- | Right-Left case (First right rotation, then left rotation)
instance (LtN l n ~ 'True, GtN rll n ~ 'True, LtN rlr rn ~ 'True,
  GtN rr rn ~ 'True, CmpNat n rln ~ 'LT, LtN l rln ~ 'True, LtN rll rln ~ 'True, CmpNat rn rln ~ 'GT, GtN rlr rln ~ 'True, GtN rr rln ~ 'True,
  CmpNat rln n ~ 'GT, CmpNat n1 n ~ 'GT, GtN rll n ~ 'True, CmpNat rn n ~ 'GT, GtN rll n ~ 'True, GtN rr n ~ 'True, GtN rlr n ~ 'True, GtN l n ~ 'True) =>
  ProofGtNRotate ('ForkTree l (Node n1 a) ('ForkTree ('ForkTree rll (Node rln rla) rlr) (Node rn ra) rr)) n 'RightUnbalanced 'LeftHeavy where
  proofGtNRotate _ _ _ _ = Refl

class Insertable (x :: Nat) (a :: Type) (t :: Tree) where
  type Insert (x :: Nat) (a :: Type) (t :: Tree) :: Tree
  insert :: Node x a -> AVL t -> AVL (Insert x a t)
instance Show a => Insertable x a 'EmptyTree where
  type Insert x a 'EmptyTree = 'ForkTree 'EmptyTree (Node x a) 'EmptyTree
  insert (Node a) EmptyAVL         = ForkAVL EmptyAVL (Node a::Node x a) EmptyAVL
instance (Insertable' x a ('ForkTree l (Node n a1) r) (CmpNat x n)) =>
  Insertable x a ('ForkTree l (Node n a1) r) where
  type Insert x a ('ForkTree l (Node n a1) r) = Insert' x a ('ForkTree l (Node n a1) r) (CmpNat x n)
  insert n t = insert' n t (Proxy::Proxy (CmpNat x n))

class Insertable' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) where
  type Insert' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) :: Tree
  insert' :: Node x a -> AVL t -> Proxy o -> AVL (Insert' x a t o)
instance (Show a, CmpNat x n ~ 'EQ) =>
  Insertable' x a ('ForkTree l (Node n a1) r) 'EQ where
  type Insert' x a ('ForkTree l (Node n a1) r) 'EQ = 'ForkTree l (Node n a) r
  insert' (Node a) (ForkAVL l (Node _) r) _ = ForkAVL l (Node a::Node n a) r
instance (Show a, CmpNat x n ~ 'LT,
  Balanceable ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n a1) r)) =>
  Insertable' x a ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  type Insert' x a ('ForkTree 'EmptyTree (Node n a1) r) 'LT = Balance ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n a1) r)
  insert' (Node a) (ForkAVL EmptyAVL n r) _ = balance (AlmostAVL (ForkAVL EmptyAVL (Node a::Node x a) EmptyAVL) n r)
instance (CmpNat x n ~ 'LT, l ~ 'ForkTree ll (Node ln lna) lr, Insertable' x a l (CmpNat x ln),
  Balanceable ('ForkTree (Insert' x a l (CmpNat x ln)) (Node n a1) r),
  ProofLtNInsert' x a l n (CmpNat x ln)) =>
  Insertable' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT where
  type Insert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT =
    Balance ('ForkTree (Insert' x a ('ForkTree ll (Node ln lna) lr) (CmpNat x ln)) (Node n a1) r)
  insert' nx (ForkAVL l@ForkAVL{} n r) _ =
    gcastWith (proofLtNInsert' nx l (Proxy::Proxy n) (Proxy::Proxy (CmpNat x ln))) $
      balance $
        AlmostAVL (insert' nx l (Proxy::Proxy (CmpNat x ln))) n r
instance (Show a, CmpNat x n ~ 'GT,
  Balanceable ('ForkTree l (Node n a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree))) =>
  Insertable' x a ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  type Insert' x a ('ForkTree l (Node n a1) 'EmptyTree) 'GT = Balance ('ForkTree l (Node n a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree))
  insert' (Node a) (ForkAVL l n EmptyAVL) _ = balance (AlmostAVL l n (ForkAVL EmptyAVL (Node a::Node x a) EmptyAVL))
instance (CmpNat x n ~ 'GT, r ~ 'ForkTree rl (Node rn rna) rr, Insertable' x a r (CmpNat x rn),
  Balanceable ('ForkTree l (Node n a1) (Insert' x a r (CmpNat x rn))),
  ProofGtNInsert' x a r n (CmpNat x rn)) =>
  Insertable' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT where
  type Insert' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT =
    Balance ('ForkTree l (Node n a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn)))
  insert' nx (ForkAVL l n r@ForkAVL{}) _ =
    gcastWith (proofGtNInsert' nx r (Proxy::Proxy n) (Proxy::Proxy (CmpNat x rn))) $
      balance $
        AlmostAVL l n (insert' nx r (Proxy::Proxy (CmpNat x rn)))

type family Member (x :: Nat) (t :: Tree) :: Bool where
  Member x 'EmptyTree = 'False
  Member x ('ForkTree l (Node n a) r) =
    (If (CmpNat x n == 'EQ)
      'True
      (If (CmpNat x n == 'LT)
        (Member x l)
        (Member x r)
      )
    )

type family LookupValueType (x :: Nat) (t :: Tree) :: Type where
  LookupValueType x ('ForkTree l (Node n a) r) =
    (If (CmpNat x n == 'EQ)
      a
      (If (CmpNat x n == 'LT)
        (LookupValueType x l)
        (LookupValueType x r)
      )
    )

class Lookupable (x :: Nat) (a :: Type) (t :: Tree) where
  lookup :: (t ~ 'ForkTree l (Node n a1) r, Member x t ~ 'True) =>
    Proxy x -> AVL t -> a
instance (Lookupable' x a ('ForkTree l (Node n a1) r) (CmpNat x n), a ~ LookupValueType x ('ForkTree l (Node n a1) r)) =>
  Lookupable x a ('ForkTree l (Node n a1) r) where
  lookup x t = lookup' x t (Proxy::Proxy (CmpNat x n))

class Lookupable' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) where
  lookup' :: (t ~ 'ForkTree l (Node n a1) r, Member x t ~ 'True) =>
    Proxy x -> AVL t -> Proxy o -> a
instance (CmpNat x n ~ 'EQ) => Lookupable' x a ('ForkTree l (Node n a) r) 'EQ where
  lookup' _ (ForkAVL _ (Node a) _) _ = getValue (Node a::Node n a)
instance (CmpNat x n ~ 'LT, l ~ 'ForkTree ll (Node ln lna) lr, Member x l ~ 'True, Lookupable' x a l (CmpNat x ln)) =>
  Lookupable' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT where
  lookup' p (ForkAVL l@ForkAVL{} _ _) _ = lookup' p l (Proxy::Proxy (CmpNat x ln))
instance (CmpNat x n ~ 'GT, r ~ 'ForkTree rl (Node rn rna) rr, Member x r ~ 'True, Lookupable' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn)) =>
  Lookupable' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT where
  lookup' p (ForkAVL _ _ r@ForkAVL{}) _ = lookup' p r (Proxy::Proxy (CmpNat x rn))

class ProofLtNDelete' (x :: Nat) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofLtNDelete' :: (t ~ 'ForkTree l n1 r, CmpNat x n ~ 'LT, LtN t n ~ 'True) =>
    Proxy x -> AVL t -> Proxy n -> Proxy o -> LtN (Delete' x t o) n :~: 'True
instance ProofLtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) 'EmptyTree) n 'EQ where
  proofLtNDelete' _ (ForkAVL EmptyAVL (Node _) EmptyAVL) _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, LtN r n ~ 'True) =>
  ProofLtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ where
  proofLtNDelete' _ (ForkAVL EmptyAVL (Node _) ForkAVL{}) _ _ = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, LtN l n ~ 'True) =>
  ProofLtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) 'EmptyTree) n 'EQ where
  proofLtNDelete' _ (ForkAVL ForkAVL{} (Node _) EmptyAVL) _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, LtN r n ~ 'True,
  l ~ 'ForkTree ll (Node ln la) lr, LtN l n ~ 'True, ProofLTMaxKey l n, Maxable l,
  ProofLtNMaxKeyDelete l n, MaxKeyDeletable l,
  ProofLtNBalance ('ForkTree (MaxKeyDelete l) (Node (MaxKey l) (MaxValue l)) r) n) =>
  ProofLtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ where
  proofLtNDelete' _ (ForkAVL l@ForkAVL{} (Node _) ForkAVL{}) _ _ =
    gcastWith (proofLtNMaxKeyDelete l (Proxy::Proxy n)) $
      gcastWith (proofLTMaxKey l (Proxy::Proxy n)) $
        gcastWith (proofLtNBalance (Proxy::Proxy ('ForkTree (MaxKeyDelete l) (Node (MaxKey l) (MaxValue l)) r)) (Proxy::Proxy n)) Refl
instance (LtN r n ~ 'True) =>
  ProofLtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofLtNDelete' _ (ForkAVL EmptyAVL (Node _) _) _ _ = Refl
instance (LtN r n ~ 'True, LtN ('ForkTree ll (Node ln la) lr) n ~ 'True, CmpNat n1 n ~ 'LT,
  ProofLtNDelete' x ('ForkTree ll (Node ln la) lr) n (CmpNat x ln),
  ProofLtNBalance ('ForkTree (Delete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln)) (Node n1 a1) r) n) =>
  ProofLtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) r) n 'LT where
  proofLtNDelete' px (ForkAVL l@ForkAVL{} _ _) _ _ =
    gcastWith (proofLtNDelete' px l (Proxy::Proxy n) (Proxy::Proxy (CmpNat x ln))) $
      gcastWith (proofLtNBalance (Proxy::Proxy ('ForkTree (Delete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln)) (Node n1 a1) r)) (Proxy::Proxy n)) Refl
instance (LtN l n ~ 'True) =>
  ProofLtNDelete' x ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofLtNDelete' _ (ForkAVL _ (Node _) EmptyAVL) _ _ = Refl
instance (LtN l n ~ 'True, CmpNat x n1 ~ 'GT, CmpNat n1 n ~ 'LT, LtN ('ForkTree rl (Node rn ra) rr) n ~ 'True,
  ProofLtNDelete' x ('ForkTree rl (Node rn ra) rr) n (CmpNat x rn),
  ProofLtNBalance ('ForkTree l (Node n1 a1) (Delete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn))) n) =>
  ProofLtNDelete' x ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'GT where
  proofLtNDelete' px (ForkAVL _ (Node _) r@ForkAVL{}) _ _ =
    gcastWith (proofLtNDelete' px r (Proxy::Proxy n) (Proxy::Proxy (CmpNat x rn))) $
      gcastWith (proofLtNBalance (Proxy::Proxy ('ForkTree l (Node n1 a1) (Delete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn)))) (Proxy::Proxy n)) Refl

class ProofGtNDelete' (x :: Nat) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofGtNDelete' :: (t ~ 'ForkTree l n1 r, CmpNat x n ~ 'GT, GtN t n ~ 'True) =>
    Proxy x -> AVL t -> Proxy n -> Proxy o -> GtN (Delete' x t o) n :~: 'True
instance ProofGtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) 'EmptyTree) n 'EQ where
  proofGtNDelete' _ (ForkAVL EmptyAVL (Node _) EmptyAVL) _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, GtN r n ~ 'True) =>
  ProofGtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ where
  proofGtNDelete' _ (ForkAVL EmptyAVL (Node _) ForkAVL{}) _ _ = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, GtN l n ~ 'True) =>
  ProofGtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) 'EmptyTree) n 'EQ where
  proofGtNDelete' _ (ForkAVL ForkAVL{} (Node _) EmptyAVL) _ _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, GtN r n ~ 'True,
  l ~ 'ForkTree ll (Node ln la) lr, CmpNat ln n ~ 'GT, GtN l n ~ 'True, t ~ 'ForkTree l (Node n1 a1) r,
  ProofGTMaxKey l n, Maxable l, ProofGtNMaxKeyDelete l n, MaxKeyDeletable l,
  ProofGtNBalance ('ForkTree (MaxKeyDelete l) (Node (MaxKey l) (MaxValue l)) r) n) =>
  ProofGtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'EQ where
  proofGtNDelete' _ (ForkAVL l@ForkAVL{} (Node _) ForkAVL{}) _ _ =
    gcastWith (proofGtNMaxKeyDelete l (Proxy::Proxy n)) $
      gcastWith (proofGTMaxKey l (Proxy::Proxy n)) $
        gcastWith (proofGtNBalance (Proxy::Proxy ('ForkTree (MaxKeyDelete l) (Node (MaxKey l) (MaxValue l)) r)) (Proxy::Proxy n)) Refl
instance (GtN r n ~ 'True) =>
  ProofGtNDelete' x ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofGtNDelete' _ (ForkAVL EmptyAVL (Node _) _) _ _ = Refl
instance (GtN r n ~ 'True, GtN ('ForkTree ll (Node ln la) lr) n ~ 'True, CmpNat n1 n ~ 'GT,
  ProofGtNDelete' x ('ForkTree ll (Node ln la) lr) n (CmpNat x ln),
  ProofGtNBalance ('ForkTree (Delete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln)) (Node n1 a1) r) n) =>
  ProofGtNDelete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a1) r) n 'LT where
  proofGtNDelete' px (ForkAVL l@ForkAVL{} _ _) _ _ =
    gcastWith (proofGtNDelete' px l (Proxy::Proxy n) (Proxy::Proxy (CmpNat x ln))) $
      gcastWith (proofGtNBalance (Proxy::Proxy ('ForkTree (Delete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln)) (Node n1 a1) r)) (Proxy::Proxy n)) Refl
instance (GtN l n ~ 'True) =>
  ProofGtNDelete' x ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofGtNDelete' _ (ForkAVL _ (Node _) EmptyAVL) _ _ = Refl
instance (GtN l n ~ 'True, CmpNat x n1 ~ 'GT, CmpNat n1 n ~ 'GT, GtN ('ForkTree rl (Node rn ra) rr) n ~ 'True,
  ProofGtNDelete' x ('ForkTree rl (Node rn ra) rr) n (CmpNat x rn),
  ProofGtNBalance ('ForkTree l (Node n1 a1) (Delete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn))) n) =>
  ProofGtNDelete' x ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn ra) rr)) n 'GT where
  proofGtNDelete' px (ForkAVL _ (Node _) r@ForkAVL{}) _ _ =
    gcastWith (proofGtNDelete' px r (Proxy::Proxy n) (Proxy::Proxy (CmpNat x rn))) $
      gcastWith (proofGtNBalance (Proxy::Proxy ('ForkTree l (Node n1 a1) (Delete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn)))) (Proxy::Proxy n)) Refl

class ProofLtNMaxKeyDeleteMaxKey (t :: Tree) where
  proofLtNMaxKeyDeleteMaxKey :: (MaxKeyDeletable t, Maxable t) =>
    AVL t -> LtN (MaxKeyDelete t) (MaxKey t) :~: 'True
instance ProofLtNMaxKeyDeleteMaxKey ('ForkTree 'EmptyTree (Node n a) 'EmptyTree) where
  proofLtNMaxKeyDeleteMaxKey (ForkAVL EmptyAVL (Node _) EmptyAVL) = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, LtN l n ~ 'True) =>
  ProofLtNMaxKeyDeleteMaxKey ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) 'EmptyTree) where
  proofLtNMaxKeyDeleteMaxKey (ForkAVL ForkAVL{} (Node _) EmptyAVL) = Refl
instance (l ~ 'ForkTree ll (Node ln la) lr, LtN l n ~ 'True, r ~ 'ForkTree rl (Node rn ra) rr,
  Maxable r, MaxKeyDeletable r, ProofLtNMaxKeyDeleteMaxKey r,
  t ~ 'ForkTree l (Node n a) r, GtN r n ~ 'True,
  LtN l (MaxKey r) ~ 'True, CmpNat n (MaxKey r) ~ 'LT) =>
  ProofLtNMaxKeyDeleteMaxKey ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) ('ForkTree rl (Node rn ra) rr)) where
  proofLtNMaxKeyDeleteMaxKey (ForkAVL ForkAVL{} (Node _) r@ForkAVL{}) =
    gcastWith (proofLtNMaxKeyDeleteMaxKey r) Refl

class ProofGtNMaxKey (t :: Tree) where
  proofGtNMaxKey :: (t ~ 'ForkTree l (Node n a) r, Maxable l) =>
    AVL t -> GtN r (MaxKey l) :~: 'True
instance ProofGtNMaxKey ('ForkTree l (Node n a) 'EmptyTree) where
  proofGtNMaxKey (ForkAVL _ (Node _) EmptyAVL) = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, GtN r n ~ 'True, LtN l n ~ 'True,
  GtN r (MaxKey l) ~ 'True) =>
  ProofGtNMaxKey ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) where
  proofGtNMaxKey (ForkAVL _ (Node _) ForkAVL{}) = Refl

class ProofGTMaxKey (t :: Tree) (n :: Nat) where
  proofGTMaxKey :: (Maxable t, GtN t n ~ 'True) =>
    AVL t -> Proxy n -> CmpNat (MaxKey t) n :~: 'GT
instance (CmpNat n1 n ~ 'GT) =>
  ProofGTMaxKey ('ForkTree l (Node n1 a) 'EmptyTree) n where
  proofGTMaxKey (ForkAVL _ (Node _) EmptyAVL) _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, GtN r n ~ 'True,
  Maxable r, ProofGTMaxKey r n) =>
  ProofGTMaxKey ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofGTMaxKey (ForkAVL _ (Node _) r@ForkAVL{}) pn = gcastWith (proofGTMaxKey r pn) Refl

class ProofGtNMaxKeyDelete (t :: Tree) (n :: Nat) where
  proofGtNMaxKeyDelete :: (MaxKeyDeletable t, GtN t n ~ 'True) =>
    AVL t -> Proxy n -> GtN (MaxKeyDelete t) n :~: 'True
instance (t ~ 'ForkTree l (Node n1 a) 'EmptyTree, GtN t n ~ 'True, GtN l n ~ 'True) =>
  ProofGtNMaxKeyDelete ('ForkTree l (Node n1 a) 'EmptyTree) n where
  proofGtNMaxKeyDelete (ForkAVL _ (Node _) EmptyAVL) _ = Refl
instance (t ~ 'ForkTree l (Node n1 a) 'EmptyTree, GtN t n ~ 'True, GtN l n ~ 'True,
  r ~ 'ForkTree rl (Node rn ra) rr, ProofGtNMaxKeyDelete r n, MaxKeyDeletable r) =>
  ProofGtNMaxKeyDelete ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofGtNMaxKeyDelete (ForkAVL _ (Node _) r@ForkAVL{}) pn = gcastWith (proofGtNMaxKeyDelete r pn) Refl

class ProofLTMaxKey (t :: Tree) (n :: Nat) where
  proofLTMaxKey :: (Maxable t, LtN t n ~ 'True) =>
    AVL t -> Proxy n -> CmpNat (MaxKey t) n :~: 'LT
instance (CmpNat n1 n ~ 'LT) =>
  ProofLTMaxKey ('ForkTree l (Node n1 a) 'EmptyTree) n where
  proofLTMaxKey (ForkAVL _ (Node _) EmptyAVL) _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, LtN r n ~ 'True,
  Maxable r, ProofLTMaxKey r n) =>
  ProofLTMaxKey ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofLTMaxKey (ForkAVL _ (Node _) r@ForkAVL{}) pn = gcastWith (proofLTMaxKey r pn) Refl

class ProofLtNMaxKeyDelete (t :: Tree) (n :: Nat) where
  proofLtNMaxKeyDelete :: (MaxKeyDeletable t, LtN t n ~ 'True) =>
    AVL t -> Proxy n -> LtN (MaxKeyDelete t) n :~: 'True
instance (t ~ 'ForkTree l (Node n1 a) 'EmptyTree, LtN t n ~ 'True, LtN l n ~ 'True) =>
  ProofLtNMaxKeyDelete ('ForkTree l (Node n1 a) 'EmptyTree) n where
  proofLtNMaxKeyDelete (ForkAVL _ (Node _) EmptyAVL) _ = Refl
instance (t ~ 'ForkTree l (Node n1 a) 'EmptyTree, LtN t n ~ 'True, LtN l n ~ 'True,
  r ~ 'ForkTree rl (Node rn ra) rr, ProofLtNMaxKeyDelete r n, MaxKeyDeletable r) =>
  ProofLtNMaxKeyDelete ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofLtNMaxKeyDelete (ForkAVL _ (Node _) r@ForkAVL{}) pn = gcastWith (proofLtNMaxKeyDelete r pn) Refl

class MaxKeyDeletable (t :: Tree) where
  type MaxKeyDelete (t :: Tree) :: Tree
  maxKeyDelete :: (t ~ 'ForkTree l (Node n a1) r) =>
    AVL t -> AVL (MaxKeyDelete t)
instance MaxKeyDeletable ('ForkTree l (Node n a1) 'EmptyTree) where
  type MaxKeyDelete ('ForkTree l (Node n a1) 'EmptyTree) = l
  maxKeyDelete (ForkAVL l (Node _) EmptyAVL) = l
instance (MaxKeyDeletable ('ForkTree rl (Node rn ra) rr),
  ProofGtNMaxKeyDelete ('ForkTree rl (Node rn ra) rr) n,
  BalancedHeights (Height l) (Height (MaxKeyDelete ('ForkTree rl (Node rn ra) rr))) ~ 'True) =>
  MaxKeyDeletable ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) where
  type MaxKeyDelete ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) =
    ('ForkTree l (Node n a1) (MaxKeyDelete ('ForkTree rl (Node rn ra) rr)))
  maxKeyDelete (ForkAVL l node r@ForkAVL{}) =
    gcastWith (proofGtNMaxKeyDelete r (Proxy::Proxy n)) $ ForkAVL l node (maxKeyDelete r)

class Maxable (t :: Tree) where
  type MaxKey (t :: Tree) :: Nat
  type MaxValue (t :: Tree) :: Type
  maxValue :: (t ~ 'ForkTree l (Node n a1) r, a ~ MaxValue t) =>
    AVL t -> a
instance Maxable ('ForkTree l (Node n a1) 'EmptyTree) where
  type MaxKey ('ForkTree l (Node n a1) 'EmptyTree) = n
  type MaxValue ('ForkTree l (Node n a1) 'EmptyTree) = a1
  maxValue (ForkAVL _ (Node a1) EmptyAVL) = a1
instance Maxable ('ForkTree rl (Node rn ra) rr) =>
  Maxable ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) where
  type MaxKey ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) = MaxKey ('ForkTree rl (Node rn ra) rr)
  type MaxValue ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) = MaxValue ('ForkTree rl (Node rn ra) rr)
  maxValue (ForkAVL _ (Node _) r@ForkAVL{}) = maxValue r

class Deletable (x :: Nat) (t :: Tree) where
  type Delete (x :: Nat) (t :: Tree) :: Tree
  delete :: Proxy x -> AVL t -> AVL (Delete x t)
instance Deletable x 'EmptyTree where
  type Delete x 'EmptyTree = 'EmptyTree
  delete _ EmptyAVL = EmptyAVL
instance (Deletable' x ('ForkTree l (Node n a1) r) (CmpNat x n)) =>
  Deletable x ('ForkTree l (Node n a1) r) where
  type Delete x ('ForkTree l (Node n a1) r) = Delete' x ('ForkTree l (Node n a1) r) (CmpNat x n)
  delete px t = delete' px t (Proxy::Proxy (CmpNat x n))

class Deletable' (x :: Nat) (t :: Tree) (o :: Ordering) where
  type Delete' (x :: Nat) (t :: Tree) (o :: Ordering) :: Tree
  delete' :: Proxy x -> AVL t -> Proxy o -> AVL (Delete' x t o)
instance Deletable' x ('ForkTree 'EmptyTree (Node n a1) 'EmptyTree) 'EQ where
  type Delete' x ('ForkTree 'EmptyTree (Node n a1) 'EmptyTree) 'EQ = 'EmptyTree
  delete' _ (ForkAVL EmptyAVL (Node _) EmptyAVL) _ = EmptyAVL
instance Deletable' x ('ForkTree 'EmptyTree (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  type Delete' x ('ForkTree 'EmptyTree (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ = ('ForkTree rl (Node rn ra) rr)
  delete' _ (ForkAVL EmptyAVL (Node _) r@ForkAVL{}) _ = r
instance Deletable' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) 'EmptyTree) 'EQ where
  type Delete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) 'EmptyTree) 'EQ = ('ForkTree ll (Node ln la) lr)
  delete' _ (ForkAVL l@ForkAVL{} (Node _) EmptyAVL) _ = l
instance (Show (MaxValue ('ForkTree ll (Node ln la) lr)), MaxKeyDeletable ('ForkTree ll (Node ln la) lr), Maxable ('ForkTree ll (Node ln la) lr),
  Balanceable ('ForkTree (MaxKeyDelete ('ForkTree ll (Node ln la) lr)) (Node (MaxKey ('ForkTree ll (Node ln la) lr)) (MaxValue ('ForkTree ll (Node ln la) lr))) ('ForkTree rl (Node rn ra) rr)),
  ProofLtNMaxKeyDeleteMaxKey ('ForkTree ll (Node ln la) lr),
  ProofGtNMaxKey ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) ('ForkTree rl (Node rn ra) rr))) =>
  Deletable' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  type Delete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ =
    Balance ('ForkTree (MaxKeyDelete ('ForkTree ll (Node ln la) lr)) (Node (MaxKey ('ForkTree ll (Node ln la) lr)) (MaxValue ('ForkTree ll (Node ln la) lr))) ('ForkTree rl (Node rn ra) rr))
  delete' _ t@(ForkAVL l@ForkAVL{} (Node _) r@ForkAVL{}) _ =
    gcastWith (proofGtNMaxKey t) $
      gcastWith (proofLtNMaxKeyDeleteMaxKey l) $
        balance $
          AlmostAVL (maxKeyDelete l) (Node (maxValue l)::Node (MaxKey ('ForkTree ll (Node ln la) lr)) (MaxValue ('ForkTree ll (Node ln la) lr))) r
instance Deletable' x ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  type Delete' x ('ForkTree 'EmptyTree (Node n a1) r) 'LT = ('ForkTree 'EmptyTree (Node n a1) r)
  delete' _ t@(ForkAVL EmptyAVL (Node _) _) _ = t
instance (Deletable' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln),
  Balanceable ('ForkTree (Delete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln)) (Node n a1) r),
  ProofLtNDelete' x ('ForkTree ll (Node ln la) lr) n (CmpNat x ln), CmpNat x n ~ 'LT) =>
  Deletable' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) r) 'LT where
  type Delete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) r) 'LT =
    Balance ('ForkTree (Delete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln)) (Node n a1) r)
  delete' px (ForkAVL l@ForkAVL{} node r) _ =
    gcastWith (proofLtNDelete' px l (Proxy::Proxy n) (Proxy::Proxy (CmpNat x ln))) $
      balance $
        AlmostAVL (delete' px l (Proxy::Proxy (CmpNat x ln))) node r
instance Deletable' x ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  type Delete' x ('ForkTree l (Node n a1) 'EmptyTree) 'GT = ('ForkTree l (Node n a1) 'EmptyTree)
  delete' _ t@(ForkAVL _ (Node _) EmptyAVL) _ = t
instance (Deletable' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn),
  Balanceable ('ForkTree l (Node n a1) (Delete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn))),
  ProofGtNDelete' x ('ForkTree rl (Node rn ra) rr) n (CmpNat x rn), CmpNat x n ~ 'GT) =>
  Deletable' x ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'GT where
  type Delete' x ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'GT =
    Balance ('ForkTree l (Node n a1) (Delete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn)))
  delete' px (ForkAVL l node r@ForkAVL{}) _ =
      gcastWith (proofGtNDelete' px r (Proxy::Proxy n) (Proxy::Proxy (CmpNat x rn))) $
        balance $
          AlmostAVL l node (delete' px r (Proxy::Proxy (CmpNat x rn)))
