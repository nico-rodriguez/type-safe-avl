{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Extern.AVLOperations where

import           Data.Kind
import           Data.Proxy
import           Data.Type.Bool
import           Extern.BSTOperations (MaxKey, MaxKeyDeletable (..),
                                       MaxKeyDelete, MaxValue (..), Maxable)
import           GHC.TypeLits
import           ITree
import           Node


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
