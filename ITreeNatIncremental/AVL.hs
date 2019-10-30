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

module ITreeNatIncremental.AVL where

import           Data.Kind
import           Data.Proxy
import           Data.Type.Bool
import           Data.Type.Equality
import           GHC.TypeLits
import           ITreeNatIncremental.BST (IsBST)
import           ITreeNatIncremental.ITree (ITree (..), Tree (..), LtN, GtN, Member, Lookupable, MaxKeyDeletable(..), Maxable(..), lookup)
import           ITreeNatIncremental.Node
import           Prelude                   hiding (lookup)
import           Unsafe.Coerce


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

type family IsAVL (t :: Tree) :: Bool where
  IsAVL 'EmptyTree = 'True
  IsAVL ('ForkTree l (Node n a) r) =
    BalancedHeights (Height l) (Height r) && IsAVL l && IsAVL r


data AVL :: Tree -> Type where
  AVL :: (IsBST t ~ 'True, IsAVL t ~ 'True) => ITree t -> AVL t

instance Show (AVL t) where
  show (AVL t) = "AVL $ " ++ show t

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
  balance :: ITree t -> ITree (Balance t)
instance Balanceable 'EmptyTree where
  type Balance 'EmptyTree = 'EmptyTree
  balance EmptyITree = EmptyITree
instance (Balanceable' ('ForkTree l (Node n a) r) (UnbalancedState (Height l) (Height r))) =>
  Balanceable ('ForkTree l (Node n a) r) where
  type Balance ('ForkTree l (Node n a) r) = Balance' ('ForkTree l (Node n a) r) (UnbalancedState (Height l) (Height r))
  balance t@(ForkITree _ (Node _) _) = balance' t (Proxy::Proxy (UnbalancedState (Height l) (Height r)))

class Balanceable' (t :: Tree) (us :: US) where
  type Balance' (t :: Tree) (us :: US) :: Tree
  balance' :: ITree t -> Proxy us -> ITree (Balance' t us)
instance Balanceable' ('ForkTree l (Node n a) r) 'NotUnbalanced where
  type Balance' ('ForkTree l (Node n a) r) 'NotUnbalanced = ('ForkTree l (Node n a) r)
  balance' t@(ForkITree _ (Node _) _) _ = t
instance Rotateable ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced (BalancedState (Height ll) (Height lr)) =>
  Balanceable' ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced where
  type Balance' ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced =
    Rotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced (BalancedState (Height ll) (Height lr))
  balance' t@(ForkITree _ (Node _) _) pus = rotate t pus (Proxy::Proxy (BalancedState (Height ll) (Height lr)))
instance Rotateable ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced (BalancedState (Height rl) (Height rr)) =>
  Balanceable' ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced where
  type Balance' ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced =
    Rotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced (BalancedState (Height rl) (Height rr))
  balance' t@(ForkITree _ (Node _) _) pus = rotate t pus (Proxy::Proxy (BalancedState (Height rl) (Height rr)))

class Rotateable (t :: Tree) (us :: US) (bs :: BS) where
  type Rotate (t :: Tree) (us :: US) (bs :: BS) :: Tree
  rotate :: ITree t -> Proxy us -> Proxy bs -> ITree (Rotate t us bs)
-- | Left-Left case (Right rotation)
instance Rotateable ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'LeftHeavy where
  type Rotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'LeftHeavy =
    ('ForkTree ll (Node ln la) ('ForkTree lr (Node n a) r))
  rotate (ForkITree (ForkITree ll lnode lr) xnode r) _ _ = ForkITree ll lnode (ForkITree lr xnode r)
instance Rotateable ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'Balanced where
  type Rotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'Balanced =
    ('ForkTree ll (Node ln la) ('ForkTree lr (Node n a) r))
  rotate (ForkITree (ForkITree ll lnode lr) xnode r) _ _ = ForkITree ll lnode (ForkITree lr xnode r)
-- | Right-Right case (Left rotation)
instance Rotateable ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'RightHeavy where
  type Rotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'RightHeavy =
    ('ForkTree ('ForkTree l (Node n a) rl) (Node rn ra) rr)
  rotate (ForkITree l xnode (ForkITree rl rnode rr)) _ _ = ForkITree (ForkITree l xnode rl) rnode rr
instance Rotateable ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'Balanced where
  type Rotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'Balanced =
    ('ForkTree ('ForkTree l (Node n a) rl) (Node rn ra) rr)
  rotate (ForkITree l xnode (ForkITree rl rnode rr)) _ _ = ForkITree (ForkITree l xnode rl) rnode rr
-- | Left-Right case (First left rotation, then right rotation)
instance Rotateable ('ForkTree ('ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr)) (Node n a) r) 'LeftUnbalanced 'RightHeavy where
  type Rotate ('ForkTree ('ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr)) (Node n a) r) 'LeftUnbalanced 'RightHeavy =
    ('ForkTree ('ForkTree ll (Node ln la) lrl) (Node lrn lra) ('ForkTree lrr (Node n a) r))
  rotate (ForkITree (ForkITree ll lnode (ForkITree lrl lrnode lrr)) xnode r) _ _ =
    ForkITree (ForkITree ll lnode lrl) lrnode (ForkITree lrr xnode r)
-- | Right-Left case (First right rotation, then left rotation)
instance Rotateable ('ForkTree l (Node n a) ('ForkTree ('ForkTree rll (Node rln rla) rlr) (Node rn ra) rr)) 'RightUnbalanced 'LeftHeavy where
  type Rotate ('ForkTree l (Node n a) ('ForkTree ('ForkTree rll (Node rln rla) rlr) (Node rn ra) rr)) 'RightUnbalanced 'LeftHeavy =
    ('ForkTree ('ForkTree l (Node n a) rll) (Node rln rla) ('ForkTree rlr (Node rn ra) rr))
  rotate (ForkITree l xnode (ForkITree (ForkITree rll rlnode rlr) rnode rr)) _ _ =
    ForkITree (ForkITree l xnode rll) rlnode (ForkITree rlr rnode rr)

class Insertable (x :: Nat) (a :: Type) (t :: Tree) where
  type Insert (x :: Nat) (a :: Type) (t :: Tree) :: Tree
  insert :: Node x a -> ITree t -> ITree (Insert x a t)
instance Show a => Insertable x a 'EmptyTree where
  type Insert x a 'EmptyTree = 'ForkTree 'EmptyTree (Node x a) 'EmptyTree
  insert (Node a) EmptyITree         = ForkITree EmptyITree (Node a::Node x a) EmptyITree
instance (Insertable' x a ('ForkTree l (Node n a1) r) (CmpNat x n)) =>
  Insertable x a ('ForkTree l (Node n a1) r) where
  type Insert x a ('ForkTree l (Node n a1) r) = Insert' x a ('ForkTree l (Node n a1) r) (CmpNat x n)
  insert n t = insert' n t (Proxy::Proxy (CmpNat x n))

class Insertable' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) where
  type Insert' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) :: Tree
  insert' :: Node x a -> ITree t -> Proxy o -> ITree (Insert' x a t o)
instance (Show a, CmpNat x n ~ 'EQ) =>
  Insertable' x a ('ForkTree l (Node n a1) r) 'EQ where
  type Insert' x a ('ForkTree l (Node n a1) r) 'EQ = 'ForkTree l (Node n a) r
  insert' (Node a) (ForkITree l (Node _) r) _ = ForkITree l (Node a::Node n a) r
instance (Show a, CmpNat x n ~ 'LT,
  Balanceable ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n a1) r)) =>
  Insertable' x a ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  type Insert' x a ('ForkTree 'EmptyTree (Node n a1) r) 'LT = Balance ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n a1) r)
  insert' (Node a) (ForkITree EmptyITree n r) _ = balance (ForkITree (ForkITree EmptyITree (Node a::Node x a) EmptyITree) n r)
instance (CmpNat x n ~ 'LT, l ~ 'ForkTree ll (Node ln lna) lr, Insertable' x a l (CmpNat x ln),
  Balanceable ('ForkTree (Insert' x a l (CmpNat x ln)) (Node n a1) r)) =>
  Insertable' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT where
  type Insert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT =
    Balance ('ForkTree (Insert' x a ('ForkTree ll (Node ln lna) lr) (CmpNat x ln)) (Node n a1) r)
  insert' (Node a) (ForkITree l@ForkITree{} n r) _ =
    balance (ForkITree (insert' (Node a::Node x a) l (Proxy::Proxy (CmpNat x ln))) n r)
instance (Show a, CmpNat x n ~ 'GT,
  Balanceable ('ForkTree l (Node n a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree))) =>
  Insertable' x a ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  type Insert' x a ('ForkTree l (Node n a1) 'EmptyTree) 'GT = Balance ('ForkTree l (Node n a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree))
  insert' (Node a) (ForkITree l n EmptyITree) _ = balance (ForkITree l n (ForkITree EmptyITree (Node a::Node x a) EmptyITree))
instance (CmpNat x n ~ 'GT, r ~ 'ForkTree rl (Node rn rna) rr, Insertable' x a r (CmpNat x rn),
  Balanceable ('ForkTree l (Node n a1) (Insert' x a r (CmpNat x rn)))) =>
  Insertable' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT where
  type Insert' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT =
    Balance ('ForkTree l (Node n a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn)))
  insert' (Node a) (ForkITree l n r@ForkITree{}) _ =
    balance (ForkITree l n (insert' (Node a::Node x a) r (Proxy::Proxy (CmpNat x rn))))

class ProofIsBSTInsert (x :: Nat) (a :: Type) (t :: Tree) where
  proofIsBSTInsert :: (IsBST t ~ 'True) =>
    Node x a -> ITree t -> IsBST (Insert x a t) :~: 'True
instance ProofIsBSTInsert x a 'EmptyTree where
  proofIsBSTInsert _ EmptyITree = Refl
instance ProofIsBSTInsert' x a ('ForkTree l (Node n a1) r) (CmpNat x n) => ProofIsBSTInsert x a ('ForkTree l (Node n a1) r) where
  proofIsBSTInsert node t = proofIsBSTInsert' node t (Proxy::Proxy (CmpNat x n))

class ProofIsBSTInsert' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) where
  proofIsBSTInsert' :: (t ~ 'ForkTree l (Node n a1) r) => Node x a -> ITree t -> Proxy o -> IsBST (Insert' x a t o) :~: 'True
instance (CmpNat x n ~ 'EQ, IsBST l ~ 'True, IsBST r ~ 'True, LtN l n ~ 'True, GtN r n ~ 'True) =>
  ProofIsBSTInsert' x a ('ForkTree l (Node n a1) r) 'EQ where
  proofIsBSTInsert' _ ForkITree{} _ = Refl
instance (l ~ 'EmptyTree, CmpNat x n ~ 'LT, IsBST l ~ 'True, IsBST r ~ 'True, LtN l n ~ 'True, GtN r n ~ 'True,
  ProofIsBSTBalance' ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n a1) r) (UnbalancedState 1 (Height r))) =>
  ProofIsBSTInsert' x a ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  proofIsBSTInsert' _ (ForkITree EmptyITree _ _) _ =  gcastWith (proofIsBSTBalance' (Proxy::Proxy ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n a1) r)) (Proxy::Proxy (UnbalancedState 1 (Height r)))) Refl
instance (l ~ 'ForkTree ll (Node ln lna) lr, CmpNat x n ~ 'LT, IsBST l ~ 'True, IsBST r ~ 'True, LtN l n ~ 'True, GtN r n ~ 'True, ProofIsBSTInsert' x a l (CmpNat x ln),
  ProofLtNInsert' x a l n (CmpNat x ln),
  ProofIsBSTBalance ('ForkTree (Insert' x a l (CmpNat x ln)) (Node n a1) r)) =>
  ProofIsBSTInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT where
  proofIsBSTInsert' node (ForkITree l@ForkITree{} _ _) _ =
    gcastWith (proofLtNInsert' node l (Proxy::Proxy n) (Proxy::Proxy (CmpNat x ln))) $
      gcastWith (proofIsBSTInsert' node l (Proxy::Proxy (CmpNat x ln))) $
        gcastWith (proofIsBSTBalance (Proxy::Proxy ('ForkTree (Insert' x a l (CmpNat x ln)) (Node n a1) r))) Refl
instance (r ~ 'EmptyTree, CmpNat x n ~ 'GT, IsBST l ~ 'True, IsBST r ~ 'True, LtN l n ~ 'True, GtN r n ~ 'True,
  ProofIsBSTBalance ('ForkTree l (Node n a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree))) =>
  ProofIsBSTInsert' x a ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  proofIsBSTInsert' _ (ForkITree _ _ EmptyITree) _ = gcastWith (proofIsBSTBalance (Proxy::Proxy ('ForkTree l (Node n a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree)))) Refl
instance (r ~ 'ForkTree rl (Node rn rna) rr, CmpNat x n ~ 'GT, IsBST r ~ 'True, IsBST l ~ 'True, GtN r n ~ 'True, LtN l n ~ 'True, ProofIsBSTInsert' x a r (CmpNat x rn),
  ProofGtNInsert' x a r n (CmpNat x rn), ProofIsBSTBalance ('ForkTree l (Node n a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn)))) =>
  ProofIsBSTInsert' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT where
  proofIsBSTInsert' node (ForkITree _ _ r@ForkITree{}) _ =
    gcastWith (proofGtNInsert' node r (Proxy::Proxy n) (Proxy::Proxy (CmpNat x rn))) $
      gcastWith (proofIsBSTInsert' node r (Proxy::Proxy (CmpNat x rn))) $
        gcastWith (proofIsBSTBalance (Proxy::Proxy ('ForkTree l (Node n a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn))))) Refl

class ProofLtNInsert' (x :: Nat) (a :: Type) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofLtNInsert' :: (t ~ 'ForkTree l n1 r, CmpNat x n ~ 'LT, LtN t n ~ 'True) =>
    Node x a -> ITree t -> Proxy n -> Proxy o -> LtN (Insert x a t) n :~: 'True
instance (t ~ 'ForkTree l n1 r, CmpNat x n1 ~ 'EQ, CmpNat x n ~ 'LT, CmpNat n1 n ~ 'LT, LtN l n ~ 'True, LtN r n ~ 'True) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) r) n 'EQ where
  proofLtNInsert' _ ForkITree{} _ _ = Refl
instance (t ~ 'ForkTree l n1 r, l ~ 'EmptyTree, CmpNat x n1 ~ 'LT, CmpNat x n ~ 'LT, CmpNat n1 n ~ 'LT, LtN l n ~ 'True, LtN r n ~ 'True,
  ProofLtNBalance ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n1 a1) r) n) =>
  ProofLtNInsert' x a ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofLtNInsert' _ (ForkITree EmptyITree _ _) _ _ =
    gcastWith (proofLtNBalance (Proxy::Proxy ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n1 a1) r)) (Proxy::Proxy n)) Refl
instance (t ~ 'ForkTree l (Node n1 a1) r, l ~ 'ForkTree ll (Node ln lna) lr, CmpNat x n1 ~ 'LT, CmpNat x n ~ 'LT, CmpNat n1 n ~ 'LT, LtN l n ~ 'True, LtN r n ~ 'True,
  ProofLtNInsert' x a l n (CmpNat x ln), ProofLtNBalance ('ForkTree (Insert' x a ('ForkTree ll (Node ln lna) lr) (CmpNat x ln)) (Node n1 a1) r) n) =>
  ProofLtNInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n1 a1) r) n 'LT where
  proofLtNInsert' node (ForkITree l@ForkITree{} _ _) n _ =
    gcastWith (proofLtNInsert' node l n (Proxy::Proxy (CmpNat x ln))) $
      gcastWith (proofLtNBalance (Proxy::Proxy ('ForkTree (Insert' x a ('ForkTree ll (Node ln lna) lr) (CmpNat x ln)) (Node n1 a1) r)) (Proxy::Proxy n)) Refl
instance (t ~ 'ForkTree l (Node n1 a1) r, r ~ 'EmptyTree, CmpNat x n1 ~ 'GT, CmpNat x n ~ 'LT, CmpNat n1 n ~ 'LT, LtN l n ~ 'True, LtN r n ~ 'True,
  ProofLtNBalance ('ForkTree l (Node n1 a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree)) n) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofLtNInsert' _ (ForkITree _ _ EmptyITree) _ _ =
    gcastWith (proofLtNBalance (Proxy::Proxy ('ForkTree l (Node n1 a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree))) (Proxy::Proxy n)) Refl
instance (t ~ 'ForkTree l (Node n1 a1) r, r ~ 'ForkTree rl (Node rn rna) rr, CmpNat x n1 ~ 'GT, CmpNat x n ~ 'LT, CmpNat n1 n ~ 'LT, LtN l n ~ 'True, LtN r n ~ 'True,
  ProofLtNInsert' x a r n (CmpNat x rn),
  ProofLtNBalance ('ForkTree l (Node n1 a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn))) n) =>
  ProofLtNInsert' x a ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn rna) rr)) n 'GT where
  proofLtNInsert' node (ForkITree _ _ r@ForkITree{}) n _ =
    gcastWith (proofLtNInsert' node r n (Proxy::Proxy (CmpNat x rn))) $
      gcastWith (proofLtNBalance (Proxy::Proxy ('ForkTree l (Node n1 a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn)))) (Proxy::Proxy n)) Refl

class ProofGtNInsert' (x :: Nat) (a :: Type) (t :: Tree) (n :: Nat) (o :: Ordering) where
  proofGtNInsert' :: (t ~ 'ForkTree l n1 r, CmpNat x n ~ 'GT, GtN t n ~ 'True) =>
    Node x a -> ITree t -> Proxy n -> Proxy o -> GtN (Insert x a t) n :~: 'True
instance (t ~ 'ForkTree l (Node n1 a1) r, CmpNat x n1 ~ 'EQ, CmpNat x n ~ 'GT, CmpNat n1 n ~ 'GT, GtN l n ~ 'True, GtN r n ~ 'True) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) r) n 'EQ where
  proofGtNInsert' _ ForkITree{} _ _ = Refl
instance (t ~ 'ForkTree l (Node n1 a1) r, l ~ 'EmptyTree, CmpNat x n1 ~ 'LT, CmpNat x n ~ 'GT, CmpNat n1 n ~ 'GT, GtN l n ~ 'True, GtN r n ~ 'True,
  ProofGtNBalance ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n1 a1) r) n) =>
  ProofGtNInsert' x a ('ForkTree 'EmptyTree (Node n1 a1) r) n 'LT where
  proofGtNInsert' _ (ForkITree EmptyITree _ _) _ _ =
    gcastWith (proofGtNBalance (Proxy::Proxy ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n1 a1) r)) (Proxy::Proxy n)) Refl
instance (t ~ 'ForkTree l (Node n1 a1) r, l ~ 'ForkTree ll (Node ln lna) lr, CmpNat x n1 ~ 'LT, CmpNat x n ~ 'GT, CmpNat n1 n ~ 'GT, GtN l n ~ 'True, GtN r n ~ 'True,
  ProofGtNInsert' x a l n (CmpNat x ln),
  ProofGtNBalance ('ForkTree (Insert' x a ('ForkTree ll (Node ln lna) lr) (CmpNat x ln)) (Node n1 a1) r) n) =>
  ProofGtNInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n1 a1) r) n 'LT where
  proofGtNInsert' x (ForkITree l@ForkITree{} _ _) n _ =
    gcastWith (proofGtNInsert' x l n (Proxy::Proxy (CmpNat x ln))) $
      gcastWith (proofGtNBalance (Proxy::Proxy ('ForkTree (Insert' x a ('ForkTree ll (Node ln lna) lr) (CmpNat x ln)) (Node n1 a1) r)) (Proxy::Proxy n)) Refl
instance (t ~ 'ForkTree l (Node n1 a1) r, r ~ 'EmptyTree, CmpNat x n1 ~ 'GT, CmpNat x n ~ 'GT, CmpNat n1 n ~ 'GT, GtN l n ~ 'True, GtN r n ~ 'True,
  ProofGtNBalance ('ForkTree l (Node n1 a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree)) n) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) 'EmptyTree) n 'GT where
  proofGtNInsert' _ (ForkITree _ _ EmptyITree) _ _ =
    gcastWith (proofGtNBalance (Proxy::Proxy ('ForkTree l (Node n1 a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree))) (Proxy::Proxy n)) Refl
instance (t ~ 'ForkTree l (Node n1 a1) r, r ~ 'ForkTree rl (Node rn rna) rr, CmpNat x n1 ~ 'GT, CmpNat x n ~ 'GT, CmpNat n1 n ~ 'GT, GtN l n ~ 'True, GtN r n ~ 'True,
  ProofGtNInsert' x a r n (CmpNat x rn),
  ProofGtNBalance ('ForkTree l (Node n1 a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn))) n) =>
  ProofGtNInsert' x a ('ForkTree l (Node n1 a1) ('ForkTree rl (Node rn rna) rr)) n 'GT where
  proofGtNInsert' x (ForkITree _ _ r@ForkITree{}) n _ =
    gcastWith (proofGtNInsert' x r n (Proxy::Proxy (CmpNat x rn))) $
      gcastWith (proofGtNBalance (Proxy::Proxy ('ForkTree l (Node n1 a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn)))) (Proxy::Proxy n)) Refl

class ProofIsBSTBalance (t :: Tree) where
  proofIsBSTBalance :: (IsBST t ~ 'True) =>
    Proxy t -> IsBST (Balance t) :~: 'True
instance ProofIsBSTBalance 'EmptyTree where
  proofIsBSTBalance _ = Refl
instance (ProofIsBSTBalance' ('ForkTree l (Node n a) r) (UnbalancedState (Height l) (Height r))) =>
  ProofIsBSTBalance ('ForkTree l (Node n a) r) where
  proofIsBSTBalance pt = gcastWith (proofIsBSTBalance' pt (Proxy::Proxy (UnbalancedState (Height l) (Height r)))) Refl

class ProofIsBSTBalance' (t :: Tree) (us :: US) where
  proofIsBSTBalance' :: Proxy t -> Proxy us -> IsBST (Balance' t us) :~: 'True
instance (IsBST ('ForkTree l (Node n a) r) ~ 'True) =>
  ProofIsBSTBalance' ('ForkTree l (Node n a) r) 'NotUnbalanced where
  proofIsBSTBalance' _ _ = Refl
instance (ProofIsBSTRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced (BalancedState (Height ll) (Height lr))) =>
  ProofIsBSTBalance' ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced where
  proofIsBSTBalance' pt pus = gcastWith (proofIsBSTRotate pt pus (Proxy::Proxy (BalancedState (Height ll) (Height lr)))) Refl
instance ProofIsBSTRotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced (BalancedState (Height rl) (Height rr)) =>
  ProofIsBSTBalance' ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced where
  proofIsBSTBalance' pt pus = gcastWith (proofIsBSTRotate pt pus (Proxy::Proxy (BalancedState (Height rl) (Height rr)))) Refl

class ProofIsBSTRotate (t :: Tree) (us::US) (bs::BS) where
  proofIsBSTRotate :: Proxy t -> Proxy us -> Proxy bs -> IsBST (Rotate t us bs) :~: 'True
-- | Left-Left case (Right rotation)
instance (IsBST ll ~ 'True, IsBST lr ~ 'True, IsBST r ~ 'True, LtN lr n ~ 'True, GtN r n ~ 'True, LtN ll ln ~ 'True, CmpNat n ln ~ 'GT,
  GtN lr ln ~ 'True, GtN r ln ~ 'True) =>
  ProofIsBSTRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'LeftHeavy where
  proofIsBSTRotate _ _ _ = Refl
instance (IsBST ll ~ 'True, IsBST lr ~ 'True, IsBST r ~ 'True, LtN lr n ~ 'True, GtN r n ~ 'True, LtN ll ln ~ 'True, CmpNat n ln ~ 'GT,
  GtN lr ln ~ 'True, GtN r ln ~ 'True) =>
  ProofIsBSTRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'Balanced where
  proofIsBSTRotate _ _ _ = Refl
-- | Right-Right case (Left rotation)
instance (IsBST l ~ 'True, IsBST rl ~ 'True, LtN l n ~ 'True, GtN rl n ~ 'True, IsBST rr ~ 'True, CmpNat n rn ~ 'LT, LtN l rn ~ 'True,
  LtN rl rn ~ 'True, GtN rr rn ~ 'True) =>
  ProofIsBSTRotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'RightHeavy where
  proofIsBSTRotate _ _ _ = Refl
instance (IsBST l ~ 'True, IsBST rl ~ 'True, LtN l n ~ 'True, GtN rl n ~ 'True, IsBST rr ~ 'True, CmpNat n rn ~ 'LT, LtN l rn ~ 'True,
  LtN rl rn ~ 'True, GtN rr rn ~ 'True) =>
  ProofIsBSTRotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'Balanced where
  proofIsBSTRotate _ _ _ = Refl
-- | Left-Right case (First left rotation, then right rotation)
instance (IsBST ll ~ 'True, IsBST lrl ~ 'True, LtN ll ln ~ 'True, GtN lrl ln ~ 'True, IsBST lrr ~ 'True, IsBST r ~ 'True, LtN lrr n ~ 'True,
  GtN r n ~ 'True, CmpNat ln lrn ~ 'LT, LtN ll lrn ~ 'True, LtN lrl lrn ~ 'True, CmpNat n lrn ~ 'GT, GtN lrr lrn ~ 'True, GtN r lrn ~ 'True) =>
  ProofIsBSTRotate ('ForkTree ('ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr)) (Node n a) r) 'LeftUnbalanced 'RightHeavy where
  proofIsBSTRotate _ _ _ = Refl
-- | Right-Left case (First right rotation, then left rotation)
instance (IsBST l ~ 'True, IsBST rll ~ 'True, LtN l n ~ 'True, GtN rll n ~ 'True, IsBST rlr ~ 'True, IsBST rr ~ 'True, LtN rlr rn ~ 'True,
  GtN rr rn ~ 'True, CmpNat n rln ~ 'LT, LtN l rln ~ 'True, LtN rll rln ~ 'True, CmpNat rn rln ~ 'GT, GtN rlr rln ~ 'True, GtN rr rln ~ 'True) =>
  ProofIsBSTRotate ('ForkTree l (Node n a) ('ForkTree ('ForkTree rll (Node rln rla) rlr) (Node rn ra) rr)) 'RightUnbalanced 'LeftHeavy where
  proofIsBSTRotate _ _ _ = Refl

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
instance (IsBST ll ~ 'True, IsBST lr ~ 'True, IsBST r ~ 'True, LtN lr n ~ 'True, GtN r n ~ 'True, LtN ll ln ~ 'True, CmpNat n ln ~ 'GT,
  GtN lr ln ~ 'True, GtN r ln ~ 'True, CmpNat ln n ~ 'LT, LtN ll n ~ 'True, CmpNat n1 n ~ 'LT, LtN r n ~ 'True) =>
  ProofLtNRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced 'LeftHeavy where
  proofLtNRotate _ _ _ _ = Refl
instance (IsBST ll ~ 'True, IsBST lr ~ 'True, IsBST r ~ 'True, LtN lr n ~ 'True, GtN r n ~ 'True, LtN ll ln ~ 'True, CmpNat n ln ~ 'GT,
  GtN lr ln ~ 'True, GtN r ln ~ 'True, CmpNat ln n ~ 'LT, LtN ll n ~ 'True, CmpNat n1 n ~ 'LT, LtN r n ~ 'True) =>
  ProofLtNRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced 'Balanced where
  proofLtNRotate _ _ _ _ = Refl
-- | Right-Right case (Left rotation)
instance (IsBST l ~ 'True, IsBST rl ~ 'True, LtN l n ~ 'True, GtN rl n ~ 'True, IsBST rr ~ 'True, CmpNat n rn ~ 'LT, LtN l rn ~ 'True,
  LtN rl rn ~ 'True, GtN rr rn ~ 'True, CmpNat rn n ~ 'LT, CmpNat n1 n ~ 'LT, LtN rl n ~ 'True, LtN rr n ~ 'True) =>
  ProofLtNRotate ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced 'RightHeavy where
  proofLtNRotate _ _ _ _ = Refl
instance (IsBST l ~ 'True, IsBST rl ~ 'True, LtN l n ~ 'True, GtN rl n ~ 'True, IsBST rr ~ 'True, CmpNat n rn ~ 'LT, LtN l rn ~ 'True,
  LtN rl rn ~ 'True, GtN rr rn ~ 'True, CmpNat rn n ~ 'LT, CmpNat n1 n ~ 'LT, LtN rl n ~ 'True, LtN rr n ~ 'True) =>
  ProofLtNRotate ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced 'Balanced where
  proofLtNRotate _ _ _ _ = Refl
-- | Left-Right case (First left rotation, then right rotation)
instance (IsBST ll ~ 'True, IsBST lrl ~ 'True, LtN ll ln ~ 'True, GtN lrl ln ~ 'True, IsBST lrr ~ 'True, IsBST r ~ 'True, LtN lrr n ~ 'True,
  GtN r n ~ 'True, CmpNat ln lrn ~ 'LT, LtN ll lrn ~ 'True, LtN lrl lrn ~ 'True, CmpNat n lrn ~ 'GT, GtN lrr lrn ~ 'True, GtN r lrn ~ 'True,
  CmpNat lrn n ~ 'LT, CmpNat ln n ~ 'LT, LtN ll n ~ 'True, LtN lrl n ~ 'True, CmpNat n1 n ~ 'LT, LtN r n ~ 'True) =>
  ProofLtNRotate ('ForkTree ('ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr)) (Node n1 a) r) n 'LeftUnbalanced 'RightHeavy where
  proofLtNRotate _ _ _ _ = Refl
-- | Right-Left case (First right rotation, then left rotation)
instance (IsBST l ~ 'True, IsBST rll ~ 'True, LtN l n ~ 'True, GtN rll n ~ 'True, IsBST rlr ~ 'True, IsBST rr ~ 'True, LtN rlr rn ~ 'True,
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
instance (IsBST ll ~ 'True, IsBST lr ~ 'True, IsBST r ~ 'True, LtN lr n ~ 'True, GtN r n ~ 'True, LtN ll ln ~ 'True, CmpNat n ln ~ 'GT,
  GtN lr ln ~ 'True, GtN r ln ~ 'True, CmpNat ln n ~ 'GT, GtN ll n ~ 'True, CmpNat n1 n ~ 'GT, GtN lr n ~ 'True) =>
  ProofGtNRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced 'LeftHeavy where
  proofGtNRotate _ _ _ _ = Refl
instance (IsBST ll ~ 'True, IsBST lr ~ 'True, IsBST r ~ 'True, LtN lr n ~ 'True, GtN r n ~ 'True, LtN ll ln ~ 'True, CmpNat n ln ~ 'GT,
  GtN lr ln ~ 'True, GtN r ln ~ 'True, CmpNat ln n ~ 'GT, GtN ll n ~ 'True, CmpNat n1 n ~ 'GT, GtN r n ~ 'True, GtN lr n ~ 'True) =>
  ProofGtNRotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n1 a) r) n 'LeftUnbalanced 'Balanced where
  proofGtNRotate _ _ _ _ = Refl
-- | Right-Right case (Left rotation)
instance (IsBST l ~ 'True, IsBST rl ~ 'True, LtN l n ~ 'True, GtN rl n ~ 'True, IsBST rr ~ 'True, CmpNat n rn ~ 'LT, LtN l rn ~ 'True,
  LtN rl rn ~ 'True, GtN rr rn ~ 'True, CmpNat rn n ~ 'GT, CmpNat n1 n ~ 'GT, GtN rl n ~ 'True, GtN rr n ~ 'True, GtN l n ~ 'True) =>
  ProofGtNRotate ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced 'RightHeavy where
  proofGtNRotate _ _ _ _ = Refl
instance (IsBST l ~ 'True, IsBST rl ~ 'True, LtN l n ~ 'True, GtN rl n ~ 'True, IsBST rr ~ 'True, CmpNat n rn ~ 'LT, LtN l rn ~ 'True,
  LtN rl rn ~ 'True, GtN rr rn ~ 'True, CmpNat rn n ~ 'GT, CmpNat n1 n ~ 'GT, GtN rl n ~ 'True, GtN rr n ~ 'True, GtN l n ~ 'True) =>
  ProofGtNRotate ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n 'RightUnbalanced 'Balanced where
  proofGtNRotate _ _ _ _ = Refl
-- | Left-Right case (First left rotation, then right rotation)
instance (IsBST ll ~ 'True, IsBST lrl ~ 'True, LtN ll ln ~ 'True, GtN lrl ln ~ 'True, IsBST lrr ~ 'True, IsBST r ~ 'True, LtN lrr n ~ 'True,
  GtN r n ~ 'True, CmpNat ln lrn ~ 'LT, LtN ll lrn ~ 'True, LtN lrl lrn ~ 'True, CmpNat n lrn ~ 'GT, GtN lrr lrn ~ 'True, GtN r lrn ~ 'True,
  CmpNat lrn n ~ 'GT, CmpNat ln n ~ 'GT, GtN ll n ~ 'True, GtN lrl n ~ 'True, CmpNat n1 n ~ 'GT, GtN r n ~ 'True, GtN lrr n ~ 'True) =>
  ProofGtNRotate ('ForkTree ('ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr)) (Node n1 a) r) n 'LeftUnbalanced 'RightHeavy where
  proofGtNRotate _ _ _ _ = Refl
-- | Right-Left case (First right rotation, then left rotation)
instance (IsBST l ~ 'True, IsBST rll ~ 'True, LtN l n ~ 'True, GtN rll n ~ 'True, IsBST rlr ~ 'True, IsBST rr ~ 'True, LtN rlr rn ~ 'True,
  GtN rr rn ~ 'True, CmpNat n rln ~ 'LT, LtN l rln ~ 'True, LtN rll rln ~ 'True, CmpNat rn rln ~ 'GT, GtN rlr rln ~ 'True, GtN rr rln ~ 'True,
  CmpNat rln n ~ 'GT, CmpNat n1 n ~ 'GT, GtN rll n ~ 'True, CmpNat rn n ~ 'GT, GtN rll n ~ 'True, GtN rr n ~ 'True, GtN rlr n ~ 'True, GtN l n ~ 'True) =>
  ProofGtNRotate ('ForkTree l (Node n1 a) ('ForkTree ('ForkTree rll (Node rln rla) rlr) (Node rn ra) rr)) n 'RightUnbalanced 'LeftHeavy where
  proofGtNRotate _ _ _ _ = Refl

class ProofIsAVLBalance' (t :: Tree) (us::US) where
  proofIsAVLBalance' :: --(IsAVL t ~ 'True) =>
    Proxy t -> Proxy us -> IsAVL (Balance' t us) :~: 'True
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
instance (CmpNat x n ~ 'EQ, IsAVL l ~ 'True, IsAVL r ~ 'True, BalancedHeights (Height l) (Height r) ~ ' True) =>
  ProofIsAVLInsert' x a ('ForkTree l (Node n a1) r) 'EQ where
  proofIsAVLInsert' _ ForkITree{} _ = Refl
instance (l ~ 'EmptyTree, CmpNat x n ~ 'LT, IsAVL l ~ 'True, IsAVL r ~ 'True,
  ProofIsAVLBalance' ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n a1) r) (UnbalancedState 1 (Height r))) =>
  ProofIsAVLInsert' x a ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  proofIsAVLInsert' _ (ForkITree EmptyITree _ _) _ =  gcastWith (proofIsAVLBalance' (Proxy::Proxy ('ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n a1) r)) (Proxy::Proxy (UnbalancedState 1 (Height r)))) Refl
instance (l ~ 'ForkTree ll (Node ln lna) lr, CmpNat x n ~ 'LT, IsAVL l ~ 'True, IsAVL r ~ 'True, ProofIsAVLInsert' x a l (CmpNat x ln),
  ProofIsAVLBalance' ('ForkTree (Insert' x a l (CmpNat x ln)) (Node n a1) r) (UnbalancedState (Height (Insert' x a ('ForkTree ll (Node ln lna) lr) (CmpNat x ln))) (Height r))) =>
  ProofIsAVLInsert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT where
  proofIsAVLInsert' node (ForkITree l@ForkITree{} _ _) _ =
    gcastWith (proofIsAVLInsert' node l (Proxy::Proxy (CmpNat x ln))) $
      gcastWith (proofIsAVLBalance' (Proxy::Proxy ('ForkTree (Insert' x a l (CmpNat x ln)) (Node n a1) r)) (Proxy::Proxy (UnbalancedState (Height (Insert' x a ('ForkTree ll (Node ln lna) lr) (CmpNat x ln))) (Height r)))) Refl
instance (r ~ 'EmptyTree, CmpNat x n ~ 'GT, IsAVL l ~ 'True, IsAVL r ~ 'True,
  ProofIsAVLBalance' ('ForkTree l (Node n a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree)) (UnbalancedState (Height l) 1)) =>
  ProofIsAVLInsert' x a ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  proofIsAVLInsert' _ (ForkITree _ _ EmptyITree) _ = gcastWith (proofIsAVLBalance' (Proxy::Proxy ('ForkTree l (Node n a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree))) (Proxy::Proxy (UnbalancedState (Height l) 1))) Refl
instance (r ~ 'ForkTree rl (Node rn rna) rr, CmpNat x n ~ 'GT, IsAVL r ~ 'True, IsAVL l ~ 'True, ProofIsAVLInsert' x a r (CmpNat x rn),
  ProofIsAVLBalance' ('ForkTree l (Node n a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn))) (UnbalancedState (Height l) (Height (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn))))) =>
  ProofIsAVLInsert' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT where
  proofIsAVLInsert' node (ForkITree _ _ r@ForkITree{}) _ =
    gcastWith (proofIsAVLInsert' node r (Proxy::Proxy (CmpNat x rn))) $
      gcastWith (proofIsAVLBalance' (Proxy::Proxy ('ForkTree l (Node n a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn)))) (Proxy::Proxy (UnbalancedState (Height l) (Height (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn)))))) Refl

insertAVL :: (Insertable x a t, ProofIsBSTInsert x a t, ProofIsAVLInsert x a t) =>
  Node x a -> AVL t -> AVL (Insert x a t)
insertAVL x (AVL t) = gcastWith (proofIsAVLInsert x t) $ gcastWith (proofIsBSTInsert x t) AVL $ insert x t

lookupAVL :: (t ~ 'ForkTree l (Node n a1) r, Member x t ~ 'True, Lookupable x a t) =>
  Proxy x -> AVL t -> a
lookupAVL p (AVL t) = lookup p t

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

class Deletable (x :: Nat) (t :: Tree) where
  type Delete (x :: Nat) (t :: Tree) :: Tree
  delete :: Proxy x -> ITree t -> ITree (Delete x t)
instance Deletable x 'EmptyTree where
  type Delete x 'EmptyTree = 'EmptyTree
  delete _ EmptyITree = EmptyITree
instance (Deletable' x ('ForkTree l (Node n a1) r) (CmpNat x n)) =>
  Deletable x ('ForkTree l (Node n a1) r) where
  type Delete x ('ForkTree l (Node n a1) r) = Delete' x ('ForkTree l (Node n a1) r) (CmpNat x n)
  delete px t = delete' px t (Proxy::Proxy (CmpNat x n))

class Deletable' (x :: Nat) (t :: Tree) (o :: Ordering) where
  type Delete' (x :: Nat) (t :: Tree) (o :: Ordering) :: Tree
  delete' :: Proxy x -> ITree t -> Proxy o -> ITree (Delete' x t o)
instance Deletable' x ('ForkTree 'EmptyTree (Node n a1) 'EmptyTree) 'EQ where
  type Delete' x ('ForkTree 'EmptyTree (Node n a1) 'EmptyTree) 'EQ = 'EmptyTree
  delete' _ (ForkITree EmptyITree (Node _) EmptyITree) _ = EmptyITree
instance Deletable' x ('ForkTree 'EmptyTree (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  type Delete' x ('ForkTree 'EmptyTree (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ = ('ForkTree rl (Node rn ra) rr)
  delete' _ (ForkITree EmptyITree (Node _) r@ForkITree{}) _ = r
instance Deletable' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) 'EmptyTree) 'EQ where
  type Delete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) 'EmptyTree) 'EQ = ('ForkTree ll (Node ln la) lr)
  delete' _ (ForkITree l@ForkITree{} (Node _) EmptyITree) _ = l
instance (Show (MaxValue ('ForkTree ll (Node ln la) lr)), MaxKeyDeletable ('ForkTree ll (Node ln la) lr), Maxable ('ForkTree ll (Node ln la) lr),
  Balanceable ('ForkTree (MaxKeyDelete ('ForkTree ll (Node ln la) lr)) (Node (MaxKey ('ForkTree ll (Node ln la) lr)) (MaxValue ('ForkTree ll (Node ln la) lr))) ('ForkTree rl (Node rn ra) rr))) =>
  Deletable' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  type Delete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ =
    Balance ('ForkTree (MaxKeyDelete ('ForkTree ll (Node ln la) lr)) (Node (MaxKey ('ForkTree ll (Node ln la) lr)) (MaxValue ('ForkTree ll (Node ln la) lr))) ('ForkTree rl (Node rn ra) rr))
  delete' _ (ForkITree l@ForkITree{} (Node _) r@ForkITree{}) _ =
    balance $ ForkITree (maxKeyDelete l) (Node (maxValue l)::Node (MaxKey ('ForkTree ll (Node ln la) lr)) (MaxValue ('ForkTree ll (Node ln la) lr))) r
instance Deletable' x ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  type Delete' x ('ForkTree 'EmptyTree (Node n a1) r) 'LT = ('ForkTree 'EmptyTree (Node n a1) r)
  delete' _ t@(ForkITree EmptyITree (Node _) _) _ = t
instance (Deletable' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln),
  Balanceable ('ForkTree (Delete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln)) (Node n a1) r)) =>
  Deletable' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) r) 'LT where
  type Delete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) r) 'LT =
    Balance ('ForkTree (Delete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln)) (Node n a1) r)
  delete' px (ForkITree l@ForkITree{} node r) _ = balance $ ForkITree (delete' px l (Proxy::Proxy (CmpNat x ln))) node r
instance Deletable' x ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  type Delete' x ('ForkTree l (Node n a1) 'EmptyTree) 'GT = ('ForkTree l (Node n a1) 'EmptyTree)
  delete' _ t@(ForkITree _ (Node _) EmptyITree) _ = t
instance (Deletable' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn),
  Balanceable ('ForkTree l (Node n a1) (Delete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn)))) =>
  Deletable' x ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'GT where
  type Delete' x ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'GT =
    Balance ('ForkTree l (Node n a1) (Delete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn)))
  delete' px (ForkITree l node r@ForkITree{}) _ = balance $ ForkITree l node (delete' px r (Proxy::Proxy (CmpNat x rn)))

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
  GtN r n ~ 'True, ProofGTMaxKey r n) =>
  ProofLtNMaxKeyDeleteMaxKey ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) ('ForkTree rl (Node rn ra) rr)) where
  proofLtNMaxKeyDeleteMaxKey t@(ForkITree ForkITree{} (Node _) r@ForkITree{}) = gcastWith (proofMaxKeyGTLtN t (Proxy::Proxy n) (proofGTMaxKey r (Proxy::Proxy n))) (gcastWith (proofGT2LT (Proxy::Proxy (MaxKey r)) (Proxy::Proxy n) (proofGTMaxKey r (Proxy::Proxy n))) (gcastWith (proofLtNMaxKeyDeleteMaxKey r) Refl))

proofMaxKeyGTLtN :: (t ~ 'ForkTree l (Node n a) r, LtN l n ~ 'True) =>
  ITree t -> Proxy n -> CmpNat (MaxKey r) n :~: 'GT -> LtN l (MaxKey r) :~: 'True
proofMaxKeyGTLtN _ _ _ = unsafeCoerce Refl

class ProofGtNMaxKey (t :: Tree) where
  proofGtNMaxKey :: (t ~ 'ForkTree l (Node n a) r, IsBST t ~ 'True, Maxable l) =>
    ITree t -> GtN r (MaxKey l) :~: 'True
instance ProofGtNMaxKey ('ForkTree l (Node n a) 'EmptyTree) where
  proofGtNMaxKey (ForkITree _ (Node _) EmptyITree) = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, IsBST r ~ 'True, GtN r n ~ 'True, LtN l n ~ 'True, ProofLTMaxKey l n) =>
  ProofGtNMaxKey ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) where
  proofGtNMaxKey t@(ForkITree l (Node _) ForkITree{}) = gcastWith (proofMaxKeyLTGtN t (Proxy::Proxy n) (proofLTMaxKey l (Proxy::Proxy n))) Refl

proofMaxKeyLTGtN :: (t ~ 'ForkTree l (Node n a) r, GtN r n ~ 'True) =>
  ITree t -> Proxy n -> CmpNat (MaxKey l) n :~: 'LT -> GtN r (MaxKey l) :~: 'True
proofMaxKeyLTGtN _ _ _ = unsafeCoerce Refl

class ProofGTMaxKey (t :: Tree) (n :: Nat) where
  proofGTMaxKey :: (Maxable t, GtN t n ~ 'True) =>
    ITree t -> Proxy n -> CmpNat (MaxKey t) n :~: 'GT
instance (CmpNat n1 n ~ 'GT) =>
  ProofGTMaxKey ('ForkTree l (Node n1 a) 'EmptyTree) n where
  proofGTMaxKey (ForkITree _ (Node _) EmptyITree) _ = Refl
instance (r ~ 'ForkTree rl (Node rn ra) rr, GtN r n ~ 'True,
  Maxable r, ProofGTMaxKey r n) =>
  ProofGTMaxKey ('ForkTree l (Node n1 a) ('ForkTree rl (Node rn ra) rr)) n where
  proofGTMaxKey (ForkITree _ (Node _) r@ForkITree{}) pn = gcastWith (proofGTMaxKey r pn) Refl

proofGT2LT :: Proxy n1 -> Proxy n2 -> CmpNat n1 n2 :~: 'GT -> CmpNat n2 n1 :~: 'LT
proofGT2LT _ _ _ = unsafeCoerce Refl

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
instance (r ~ 'ForkTree rl (Node rn ra) rr, LtN r n ~ 'True,
  Maxable r, ProofLTMaxKey r n) =>
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

deleteAVL :: (Deletable x t, ProofIsBSTDelete x t, ProofIsAVLDelete x t) =>
  Proxy x -> AVL t -> AVL (Delete x t)
deleteAVL px (AVL t) = gcastWith (proofIsAVLDelete px t) $ gcastWith (proofIsBSTDelete px t) (AVL $ delete px t)
