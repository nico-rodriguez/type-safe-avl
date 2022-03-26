{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Tree.AVL.Extern.Constructor
-- Description : Constructor of type safe externalist AVL trees
-- Copyright   : (c) Nicolás Rodríguez, 2021
-- License     : GPL-3
-- Maintainer  : Nicolás Rodríguez
-- Stability   : experimental
-- Portability : POSIX
--
-- Implementation of the constructor of type safe externalist AVL
-- trees and instance definition for the `Show` type class.
module Data.Tree.AVL.Extern.Constructors
  ( AVL (AVL),
    mkAVL,
    IsBalancedT (..),
    IsBalancedC (..),
    IsAlmostBalancedT (..),
  )
where

import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Tree.AVL.Invariants (BalancedHeights, Height)
import Data.Tree.BST.Extern.Constructors (IsBSTC (isBSTT), IsBSTT)
import Data.Tree.ITree (ITree, Tree (EmptyTree, ForkTree))
import Data.Tree.Node (Node)
import Prelude (Bool (True), Show (show), (++))

-- | Constructor of `AVL` trees. Given an arbitrary `ITree`, it constructs
-- a new `AVL` together with the proof terms `IsBSTT`, which shows
-- that the keys are ordered, and `IsBalancedT`, which shows that the heights are balanced.
data AVL :: Tree -> Type where
  AVL :: ITree t -> IsBSTT t -> IsBalancedT t -> AVL t

-- | Instance definition for the `Show` type class.
-- It relies on the instance for `ITree`.
instance Show (AVL t) where
  show (AVL t _ _) = "AVL $ " ++ show t

-- | Proof term which shows that @t@ is an `AVL`.
-- The restrictions on the constructor `ForkIsBalancedT`
-- are verified at compile time.
-- Given two proofs of `AVL` and an arbitrary node, it tests wether the heights of the sub trees are balanced.
-- Notice that this is all that's needed to assert that the new tree is a `AVL`,
-- since, both left and right proofs are evidence of height balance in both
-- left and right sub trees.
data IsBalancedT :: Tree -> Type where
  EmptyIsBalancedT :: IsBalancedT 'EmptyTree
  ForkIsBalancedT ::
    (BalancedHeights (Height l) (Height r) n ~ 'True) =>
    IsBalancedT l ->
    Proxy (Node n a) ->
    IsBalancedT r ->
    IsBalancedT ('ForkTree l (Node n a) r)

-- | Type class for automatically constructing the proof term `IsBalancedT`.
class IsBalancedC (t :: Tree) where
  isBalancedT :: IsBalancedT t

-- | Instances for the type class `IsBalancedC`.
instance IsBalancedC 'EmptyTree where
  isBalancedT = EmptyIsBalancedT

instance
  (IsBalancedC l, IsBalancedC r, BalancedHeights (Height l) (Height r) n ~ 'True) =>
  IsBalancedC ('ForkTree l (Node n a) r)
  where
  isBalancedT = ForkIsBalancedT isBalancedT (Proxy :: Proxy (Node n a)) isBalancedT

-- | Given an `ITree`, compute the proof terms `IsBSTT` and `IsBalancedT`, through
-- the type classes `IsBSTC` and `IsBalancedC` in order to check if it is an `AVL` tree.
-- This is the fully externalist constructor for `AVL` trees.
mkAVL :: (IsBSTC t, IsBalancedC t) => ITree t -> AVL t
mkAVL t = AVL t isBSTT isBalancedT

-- | Proof term which shows that @t@ is an @Almost AVL@.
data IsAlmostBalancedT :: Tree -> Type where
  ForkIsAlmostBalancedT :: IsBalancedT l -> Proxy (Node n a) -> IsBalancedT r -> IsAlmostBalancedT ('ForkTree l (Node n a) r)
