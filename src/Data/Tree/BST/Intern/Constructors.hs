{-|
Module      : Data.Tree.BST.Intern.Constructor
Description : Constructor of type safe internalist BST trees
Copyright   : (c) Nicolás Rodríguez, 2021
License     : GPL-3
Maintainer  : Nicolás Rodríguez
Stability   : experimental
Portability : POSIX

Implementation of the constructor of type safe internalist BST
trees and instance definition for the @Show@ type class.
-}

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TypeFamilies       #-}

{-# LANGUAGE Safe               #-}

module Data.Tree.BST.Intern.Constructors (
  BST(EmptyBST,ForkBST),
) where

import           Data.Kind                (Type)
import           Data.Tree.BST.Invariants (GtN, LtN)
import           Data.Tree.ITree          (Tree (EmptyTree, ForkTree))
import           Data.Tree.Node           (Node)
import           Prelude                  (Bool (True), Show (show), String,
                                           (++))


-- | Constructor of internalist `BST` trees.
-- Given two `BST` trees and an arbitrary node, it tests at compile time wether the key
-- of the node verifies the `LtN` and `GtN` invariants with respect to each tree.
-- Notice that this is all that's needed to assert that the new tree is a `BST`,
-- since, by recursive logic, both left and right BST trees already respect the key ordering.
data BST :: Tree -> Type where
  EmptyBST :: BST 'EmptyTree
  ForkBST  :: (Show a, LtN l n ~ 'True, GtN r n ~ 'True) =>
    BST l -> Node n a -> BST r -> BST ('ForkTree l (Node n a) r)

-- | Instance definition for the `Show` type class.
instance Show (BST t) where
  show EmptyBST         = "E"
  show (ForkBST l n r)  = "F " ++ go l ++ " " ++ show n ++ " " ++ go r
    where
      go :: BST t' -> String
      go EmptyBST         = "E"
      go (ForkBST l' n' r')  = "(F " ++ go l' ++ " " ++ show n' ++ " " ++ go r' ++ ")"
