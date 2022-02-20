{-|
Module      : Data.Tree.AVL.Extern.Constructor
Description : Constructor of type safe externalist AVL trees
Copyright   : (c) Nicolás Rodríguez, 2021
License     : GPL-3
Maintainer  : Nicolás Rodríguez
Stability   : experimental
Portability : POSIX

Implementation of the constructor of type safe externalist AVL
trees and instance definition for the `Show` type class.
-}

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TypeFamilies       #-}

{-# LANGUAGE Safe               #-}

module Data.Tree.AVL.Extern.Constructors (
  AVL(AVL), mkAVL,
  IsAVLT(..), IsAVLC(..),
  IsAlmostAVLT(..)
) where

import           Data.Kind                (Type)
import           Data.Proxy               (Proxy (Proxy))
import           Data.Tree.Node           (Node)
import           Data.Tree.AVL.Invariants (BalancedHeights, Height)
import           Data.Tree.BST.Extern.Constructors (IsBSTT, IsBSTC(isBSTT))
import           Data.Tree.ITree          (ITree, Tree (EmptyTree,ForkTree))
import           Prelude                  (Show (show), Bool(True), (++))



-- | Constructor of `AVL` trees. Given an arbitrary `ITree`, it constructs
-- a new `AVL` together with the proof terms `IsBSTT`, which shows
-- that the keys are ordered, and `IsAVLT`, which shows that the heights are balanced.
data AVL :: Tree -> Type where
  AVL :: ITree t -> IsBSTT t -> IsAVLT t -> AVL t

-- | Instance definition for the `Show` type class.
-- It relies on the instance for `ITree`.
instance Show (AVL t) where
  show (AVL t _ _) = "AVL $ " ++ show t


-- | Proof term which shows that @t@ is an `AVL`.
-- The restrictions on the constructor `ForkIsAVLT`
-- are verified at compile time.
-- Given two proofs of `AVL` and an arbitrary node, it tests wether the heights of the sub trees are balanced.
-- Notice that this is all that's needed to assert that the new tree is a `AVL`,
-- since, both left and right proofs are evidence of height balance in both
-- left and right sub trees.
data IsAVLT :: Tree -> Type where
  EmptyIsAVLT :: IsAVLT 'EmptyTree
  ForkIsAVLT  :: (BalancedHeights (Height l) (Height r) n ~ 'True) =>
    IsAVLT l -> Proxy (Node n a) -> IsAVLT r -> IsAVLT ('ForkTree l (Node n a) r)


-- | Type class for automatically constructing the proof term `IsAVLT`.
class IsAVLC (t :: Tree) where
  isAVLT :: IsAVLT t

-- | Instances for the type class `IsAVLC`.
instance IsAVLC 'EmptyTree where
  isAVLT = EmptyIsAVLT
instance (IsAVLC l, IsAVLC r, BalancedHeights (Height l) (Height r) n ~ 'True) =>
  IsAVLC ('ForkTree l (Node n a) r) where
  isAVLT = ForkIsAVLT isAVLT (Proxy::Proxy (Node n a)) isAVLT


-- | Given an `ITree`, compute the proof terms `IsBSTT` and `IsAVLT`, through
-- the type classes `IsBSTC` and `IsAVLC` in order to check if it is an `AVL` tree.
-- This is the fully externalist constructor for `AVL` trees.
mkAVL :: (IsBSTC t, IsAVLC t) => ITree t -> AVL t
mkAVL t = AVL t isBSTT isAVLT


-- | Proof term which shows that @t@ is an @Almost AVL@.
data IsAlmostAVLT :: Tree -> Type where
  ForkIsAlmostAVLT  :: IsAVLT l -> Proxy (Node n a) -> IsAVLT r -> IsAlmostAVLT ('ForkTree l (Node n a) r)

