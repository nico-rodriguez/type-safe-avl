{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE Safe                  #-}

module Data.Tree.AVL.Invariants (
  BS(Balanced,LeftHeavy,RightHeavy),
  BalancedState,
  Height, BalancedHeights,
  US(LeftUnbalanced,NotUnbalanced,RightUnbalanced),
  UnbalancedState,
  IsAVL, IsAVLT(..), IsAVLC(..),
  IsAlmostAVLT(..)
) where

import           Data.Kind       (Type)
import           Data.Tree.ITree (Tree (EmptyTree, ForkTree))
import           Data.Tree.Node  (Node)
import           Data.Type.Bool  (type (&&), If)
import           GHC.TypeNats    (type (+), type (-), type (<=?), Nat)
import           Prelude         (Bool (False, True), undefined)


-- | Get the maximun between two type level natural numbers.
type family Max (n1 :: Nat) (n2 :: Nat) :: Nat where
  Max n1 n2 =
    (If (n1 <=? n2)
      n2
      n1
    )

-- | Get the height of a tree.
type family Height (t :: Tree) :: Nat where
  Height 'EmptyTree = 0
  Height ('ForkTree l (Node _n _a) r) = 1 + Max (Height l) (Height r)

-- | Check if two type level natural numbers,
-- | that represent the heights of some left and right sub trees,
-- | differ at most in one (i.e., the tree is balanced).
type family BalancedHeights (h1 :: Nat) (h2 :: Nat) :: Bool where
  BalancedHeights 0   0   = 'True
  BalancedHeights 1   0   = 'True
  BalancedHeights _h1 0   = 'False
  BalancedHeights 0   1   = 'True
  BalancedHeights 0   _h2 = 'False
  BalancedHeights h1  h2  = BalancedHeights (h1-1) (h2-1)


-- | Data type that represents the state of unbalance of the sub trees:
-- | - LeftUnbalanced: height(left sub tree) = height(right sub tree) + 2.
-- | - RightUnbalanced: height(right sub tree) = height(leftt sub tree) + 2.
-- | - NotUnbalanced: tree is not unbalanced.
data US = LeftUnbalanced | RightUnbalanced | NotUnbalanced

-- | Check from two type level natural numbers,
-- | that represent the heights of some left and right sub trees,
-- | if the tree is balanced or if some of those sub trees is unbalanced.
type family UnbalancedState (h1 :: Nat) (h2 :: Nat) :: US where
  UnbalancedState 0 0   = 'NotUnbalanced
  UnbalancedState 1 0   = 'NotUnbalanced
  UnbalancedState 0 1   = 'NotUnbalanced
  UnbalancedState 2 0   = 'LeftUnbalanced
  UnbalancedState 0 2   = 'RightUnbalanced
  UnbalancedState h1 h2 = UnbalancedState (h1-1) (h2-1)


-- | Data type that represents the state of balance of the sub trees in a balanced tree:
-- | - LeftHeavy: height(left sub tree) = height(right sub tree) + 1.
-- | - RightHeavy: height(right sub tree) = height(leftt sub tree) + 1.
-- | - Balanced: height(left sub tree) = height(right sub tree).
data BS = LeftHeavy | RightHeavy | Balanced

-- | Check from two type level natural numbers,
-- | that represent the heights of some left and right sub trees,
-- | if some of those sub trees have height larger than the other.
type family BalancedState (h1 :: Nat) (h2 :: Nat) :: BS where
  BalancedState 0 0   = 'Balanced
  BalancedState 1 0   = 'LeftHeavy
  BalancedState 0 1   = 'RightHeavy
  BalancedState h1 h2 = BalancedState (h1-1) (h2-1)


-- | Check if tree is AVL by comparing the differences in the heights of all sub trees pairs
-- | It doesn't check if the tree is BST, IsBST is used for that.
type family IsAVL (t :: Tree) :: Bool where
  IsAVL 'EmptyTree                   = 'True
  IsAVL ('ForkTree l (Node _n _a) r) =
    BalancedHeights (Height l) (Height r) && IsAVL l && IsAVL r

-- | Proof term which shows that `t` is an AVL
data IsAVLT :: Tree -> Type where
  EmptyIsAVLT :: IsAVLT 'EmptyTree
  ForkIsAVLT  :: (BalancedHeights (Height l) (Height r) ~ 'True) =>
    IsAVLT l -> Node n a -> IsAVLT r -> IsAVLT ('ForkTree l (Node n a) r)

-- | Class for constructing the proof term IsAVLT
class IsAVLC (t :: Tree) where
  isAVLT :: IsAVLT t

instance IsAVLC 'EmptyTree where
  isAVLT = EmptyIsAVLT
instance (IsAVLC l, IsAVLC r, BalancedHeights (Height l) (Height r) ~ 'True) =>
  IsAVLC ('ForkTree l (Node n a) r) where
  isAVLT = ForkIsAVLT isAVLT (undefined::Node n a) isAVLT

-- | Proof term which shows that `t` is a BST
data IsAlmostAVLT :: Tree -> Type where
  EmptyIsAlmostAVLT :: IsAlmostAVLT 'EmptyTree
  ForkIsAlmostAVLT  :: IsAVLT l -> Node n a -> IsAVLT r -> IsAlmostAVLT ('ForkTree l (Node n a) r)
