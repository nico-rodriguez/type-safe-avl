{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

-- |
-- Module      : Data.Tree.AVL.Extern.Balance
-- Description : Balancing algorithm over ITree trees
-- Copyright   : (c) Nicolás Rodríguez, 2021
-- License     : GPL-3
-- Maintainer  : Nicolás Rodríguez
-- Stability   : experimental
-- Portability : POSIX
--
-- Implementation of the balancing algorithm over ITree trees for externalist AlmostAVL trees.
module Data.Tree.AVL.Extern.Balance
  ( Balanceable (Balance, balance),
    Balanceable' (Balance', balance'),
    Rotateable (Rotate, rotate),
  )
where

import Data.Proxy (Proxy (Proxy))
import Data.Tree.AVL.Invariants
  ( BS (Balanced, LeftHeavy, RightHeavy),
    BalancedState,
    Height,
    US (LeftUnbalanced, NotUnbalanced, RightUnbalanced),
    UnbalancedState,
  )
import Data.Tree.ITree
  ( ITree (EmptyITree, ForkITree),
    Tree (EmptyTree, ForkTree),
  )
import Data.Tree.Node (Node)
import Prelude ()

-- | This type class provides the functionality to balance
-- a tree @t@ without checking any structural invariant (key ordering or height balance).
-- The insertion is defined at the value level and the type level;
-- the verification of the @BST@/@AVL@ restrictions is performed after the insertion.
class Balanceable (t :: Tree) where
  type Balance (t :: Tree) :: Tree
  balance :: ITree t -> ITree (Balance t)

instance Balanceable 'EmptyTree where
  type Balance 'EmptyTree = 'EmptyTree
  balance _ = EmptyITree

instance
  ( us ~ UnbalancedState (Height l) (Height r),
    Balanceable' ('ForkTree l (Node n a) r) us
  ) =>
  Balanceable ('ForkTree l (Node n a) r)
  where
  type Balance ('ForkTree l (Node n a) r) = Balance' ('ForkTree l (Node n a) r) (UnbalancedState (Height l) (Height r))
  balance t = balance' t (Proxy :: Proxy us)

-- | This type class provides the functionality to balance
-- a tree @t@ without checking any structural invariant (key ordering or height balance).
-- It's only used by the 'Balanceable' class and it has one extra parameter @us@,
-- which is the `UnbalancedState` of the two sub trees of @t@.
class Balanceable' (t :: Tree) (us :: US) where
  type Balance' (t :: Tree) (us :: US) :: Tree
  balance' :: ITree t -> Proxy us -> ITree (Balance' t us)

instance Balanceable' ('ForkTree l (Node n a) r) 'NotUnbalanced where
  type Balance' ('ForkTree l (Node n a) r) 'NotUnbalanced = ('ForkTree l (Node n a) r)
  balance' t _ = t

instance
  ( bs ~ BalancedState (Height ll) (Height lr),
    Rotateable ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced bs
  ) =>
  Balanceable' ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced
  where
  type
    Balance' ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced =
      Rotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced (BalancedState (Height ll) (Height lr))
  balance' t pus = rotate t pus (Proxy :: Proxy bs)

instance
  ( bs ~ BalancedState (Height rl) (Height rr),
    Rotateable ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced bs
  ) =>
  Balanceable' ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced
  where
  type
    Balance' ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced =
      Rotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced (BalancedState (Height rl) (Height rr))
  balance' t pus = rotate t pus (Proxy :: Proxy bs)

-- | This type class provides the functionality to apply a rotation to
-- a tree @t@ without checking any structural invariant (key ordering or height balance).
-- The rotation is defined at the value level and the type level;
-- the verification of the @BST@/@AVL@ restrictions is performed after the insertion.
class Rotateable (t :: Tree) (us :: US) (bs :: BS) where
  type Rotate (t :: Tree) (us :: US) (bs :: BS) :: Tree
  rotate :: ITree t -> Proxy us -> Proxy bs -> ITree (Rotate t us bs)

-- Left-Left case (Right rotation)
instance Rotateable ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'LeftHeavy where
  type
    Rotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'LeftHeavy =
      ('ForkTree ll (Node ln la) ('ForkTree lr (Node n a) r))
  rotate (ForkITree (ForkITree ll lnode lr) node r) _ _ = ForkITree ll lnode (ForkITree lr node r)

instance Rotateable ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'Balanced where
  type
    Rotate ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a) r) 'LeftUnbalanced 'Balanced =
      ('ForkTree ll (Node ln la) ('ForkTree lr (Node n a) r))
  rotate (ForkITree (ForkITree ll lnode lr) node r) _ _ = ForkITree ll lnode (ForkITree lr node r)

-- Right-Right case (Left rotation)
instance Rotateable ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'RightHeavy where
  type
    Rotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'RightHeavy =
      ('ForkTree ('ForkTree l (Node n a) rl) (Node rn ra) rr)
  rotate (ForkITree l node (ForkITree rl rnode rr)) _ _ = ForkITree (ForkITree l node rl) rnode rr

instance Rotateable ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'Balanced where
  type
    Rotate ('ForkTree l (Node n a) ('ForkTree rl (Node rn ra) rr)) 'RightUnbalanced 'Balanced =
      ('ForkTree ('ForkTree l (Node n a) rl) (Node rn ra) rr)
  rotate (ForkITree l node (ForkITree rl rnode rr)) _ _ = ForkITree (ForkITree l node rl) rnode rr

-- Left-Right case (First left rotation, then right rotation)
instance Rotateable ('ForkTree ('ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr)) (Node n a) r) 'LeftUnbalanced 'RightHeavy where
  type
    Rotate ('ForkTree ('ForkTree ll (Node ln la) ('ForkTree lrl (Node lrn lra) lrr)) (Node n a) r) 'LeftUnbalanced 'RightHeavy =
      ('ForkTree ('ForkTree ll (Node ln la) lrl) (Node lrn lra) ('ForkTree lrr (Node n a) r))
  rotate (ForkITree (ForkITree ll lnode (ForkITree lrl lrnode lrr)) node r) _ _ =
    ForkITree (ForkITree ll lnode lrl) lrnode (ForkITree lrr node r)

-- Right-Left case (First right rotation, then left rotation)
instance Rotateable ('ForkTree l (Node n a) ('ForkTree ('ForkTree rll (Node rln rla) rlr) (Node rn ra) rr)) 'RightUnbalanced 'LeftHeavy where
  type
    Rotate ('ForkTree l (Node n a) ('ForkTree ('ForkTree rll (Node rln rla) rlr) (Node rn ra) rr)) 'RightUnbalanced 'LeftHeavy =
      ('ForkTree ('ForkTree l (Node n a) rll) (Node rln rla) ('ForkTree rlr (Node rn ra) rr))
  rotate (ForkITree l node (ForkITree (ForkITree rll rlnode rlr) rnode rr)) _ _ =
    ForkITree (ForkITree l node rll) rlnode (ForkITree rlr rnode rr)
