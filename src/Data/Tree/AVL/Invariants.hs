{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Data.Tree.AVL.Invariants
-- Description : Type level AVL invariants
-- Copyright   : (c) Nicolás Rodríguez, 2021
-- License     : GPL-3
-- Maintainer  : Nicolás Rodríguez
-- Stability   : experimental
-- Portability : POSIX
--
-- Type level restrictions for the key ordering in type safe AVL trees.
module Data.Tree.AVL.Invariants
  ( BS (Balanced, LeftHeavy, RightHeavy),
    BalancedState,
    Height,
    BalancedHeights,
    US (LeftUnbalanced, NotUnbalanced, RightUnbalanced),
    UnbalancedState,
  )
where

import Data.Tree.ITree (Tree (EmptyTree, ForkTree))
import Data.Tree.Node (Node)
import Data.Type.Bool (If)
import GHC.TypeLits (ErrorMessage (ShowType, Text, (:<>:)), TypeError)
import GHC.TypeNats (Nat, type (+), type (-), type (<=?))
import Prelude (Bool (True))

-- | Get the maximum between two type level natural numbers.
type family Max (n1 :: Nat) (n2 :: Nat) :: Nat where
  Max n1 n2 =
    ( If
        (n1 <=? n2)
        n2
        n1
    )

-- | Get the height of a tree.
type family Height (t :: Tree) :: Nat where
  Height 'EmptyTree = 0
  Height ('ForkTree l (Node _n _a) r) = 1 + Max (Height l) (Height r)

-- | Check if two type level natural numbers,
-- that represent the heights of some left and right sub trees,
-- differ at most in one (i.e., the tree is balanced).
type family BalancedHeights (h1 :: Nat) (h2 :: Nat) (k :: Nat) :: Bool where
  BalancedHeights 0 0 _k = 'True
  BalancedHeights 1 0 _k = 'True
  BalancedHeights _h1 0 k = TypeError ('Text "The left sub tree at node with key " ':<>: 'ShowType k ':<>: 'Text " has +2 greater height!")
  BalancedHeights 0 1 _k = 'True
  BalancedHeights 0 _h2 k = TypeError ('Text "The right sub tree at node with key " ':<>: 'ShowType k ':<>: 'Text " has +2 greater height!")
  BalancedHeights h1 h2 k = BalancedHeights (h1 - 1) (h2 - 1) k

-- | Data type that represents the state of unbalance of the sub trees:
--
-- [`LeftUnbalanced`] @height(left sub tree) = height(right sub tree) + 2@
--
-- [`RightUnbalanced`] @height(right sub tree) = height(left sub tree) + 2@
--
-- [`NotUnbalanced`] @tree is not unbalanced@
data US = LeftUnbalanced | RightUnbalanced | NotUnbalanced

-- | Check from two type level natural numbers,
-- that represent the heights of some left and right sub trees,
-- if the tree is balanced or if some of those sub trees is unbalanced.
type family UnbalancedState (h1 :: Nat) (h2 :: Nat) :: US where
  UnbalancedState 0 0 = 'NotUnbalanced
  UnbalancedState 1 0 = 'NotUnbalanced
  UnbalancedState 0 1 = 'NotUnbalanced
  UnbalancedState 2 0 = 'LeftUnbalanced
  UnbalancedState 0 2 = 'RightUnbalanced
  UnbalancedState h1 h2 = UnbalancedState (h1 - 1) (h2 - 1)

-- | Data type that represents the state of balance of the sub trees in a balanced tree:
--
-- [`LeftHeavy`] @height(left sub tree) = height(right sub tree) + 1@
--
-- [`RightHeavy`] @height(right sub tree) = height(left sub tree) + 1@
--
-- [`Balanced`] @height(left sub tree) = height(right sub tree)@
data BS = LeftHeavy | RightHeavy | Balanced

-- | Check from two type level natural numbers,
-- that represent the heights of some left and right sub trees,
-- if some of those sub trees have height larger than the other.
type family BalancedState (h1 :: Nat) (h2 :: Nat) :: BS where
  BalancedState 0 0 = 'Balanced
  BalancedState 1 0 = 'LeftHeavy
  BalancedState 0 1 = 'RightHeavy
  BalancedState h1 h2 = BalancedState (h1 - 1) (h2 - 1)
