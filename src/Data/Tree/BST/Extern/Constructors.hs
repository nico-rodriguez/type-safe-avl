{-|
Module      : Data.Tree.BST.Extern.Constructor
Description : Constructor of type safe externalist BST trees
Copyright   : (c) Nicolás Rodríguez, 2021
License     : GPL-3
Maintainer  : Nicolás Rodríguez
Stability   : experimental
Portability : POSIX

Implementation of the constructor of type safe externalist BST
trees and instance definition for the `Show` type class.
-}

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TypeFamilies       #-}

{-# LANGUAGE Safe               #-}

module Data.Tree.BST.Extern.Constructors (
  BST(BST), mkBST,
  IsBSTT(..), IsBSTC(..)
) where

import           Data.Kind                (Type)
import           Data.Proxy               (Proxy(Proxy))
import           Data.Tree.Node           (Node)
import           Data.Tree.BST.Invariants (LtN, GtN)
import           Data.Tree.ITree          (ITree, Tree(EmptyTree,ForkTree))
import           Prelude                  (Show (show), Bool(True), (++))


-- | Constructor of `BST` trees. Given an arbitrary `ITree`, it constructs
-- a new `BST` together with the proof term `IsBSTT` which shows
-- that the keys are ordered.
data BST :: Tree -> Type where
  BST :: ITree t -> IsBSTT t -> BST t

-- | Instance definition for the `Show` type class.
-- It relies on the instance for `ITree`.
instance Show (BST t) where
  show (BST t _) = "BST $ " ++ show t


-- | Proof term which shows that `t` is a `BST`.
-- The restrictions on the constructor `ForkIsBSTT`
-- are verified at compile time.
-- Given two proofs of `BST` and an arbitrary node, it tests wether the key
-- of the node verifies the `LtN` and `GtN` invariants.
-- Notice that this is all that's needed to assert that the new tree is a `BST`,
-- since, both left and right proofs are evidence of the key ordering in both
-- left and right sub trees.
data IsBSTT :: Tree -> Type where
  EmptyIsBSTT :: IsBSTT 'EmptyTree
  ForkIsBSTT  :: (LtN l n ~ 'True, GtN r n ~ 'True) =>
    IsBSTT l -> Proxy (Node n a) -> IsBSTT r -> IsBSTT ('ForkTree l (Node n a) r)

-- | Type class for automatically constructing the proof term `IsBSTT`.
class IsBSTC (t :: Tree) where
  isBSTT :: IsBSTT t

-- | Instances for the type class `IsBSTC`.
instance IsBSTC 'EmptyTree where
  isBSTT = EmptyIsBSTT
instance (IsBSTC l, IsBSTC r, LtN l n ~ 'True, GtN r n ~ 'True) =>
  IsBSTC ('ForkTree l (Node n a) r) where
  isBSTT = ForkIsBSTT isBSTT (Proxy :: Proxy (Node n a)) isBSTT


-- | Given an `ITree`, compute the proof term `IsBSTT`, through the
-- type class `IsBSTC`, in order to check if it is a `BST` tree.
-- This is the fully externalist constructor for `BST` trees.
mkBST :: (IsBSTC t) => ITree t -> BST t
mkBST t = BST t isBSTT
