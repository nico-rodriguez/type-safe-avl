{-|
Module      : Data.Tree.ITree
Description : Type trees and indexed trees
Copyright   : (c) Nicolás Rodríguez, 2021
License     : GPL-3
Maintainer  : Nicolás Rodríguez
Stability   : experimental
Portability : POSIX

Definition of the trees used at the type level for the type safe BST and AVL trees.
-}

{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

{-# LANGUAGE Safe           #-}

module Data.Tree.ITree (
  Tree(..),
  ITree(..))
where

import           Data.Kind      (Type)
import           Data.Tree.Node (Node)
import           Prelude        (Show (show), String, (++))


-- | Tree definition for the type level. Type safe trees are indexed by
-- type trees of the kind `'Tree`.
data Tree :: Type where
  EmptyTree :: Tree
  ForkTree  :: Tree -> n -> Tree -> Tree

-- | Value level trees indexed by trees of kind `'Tree`.
-- The tree structure from the value level is replicated at the type level.
data ITree :: Tree -> Type where
  EmptyITree :: ITree 'EmptyTree
  ForkITree  :: Show a => ITree l -> Node n a -> ITree r -> ITree ('ForkTree l (Node n a) r)

-- | `Show` instance for `ITree`.
instance Show (ITree t) where
  show EmptyITree         = "E"
  show (ForkITree l n r)  = "F " ++ go l ++ " " ++ show n ++ " " ++ go r
    where
      go :: ITree t' -> String
      go EmptyITree         = "E"
      go (ForkITree l' n' r')  = "(F " ++ go l' ++ " " ++ show n' ++ " " ++ go r' ++ ")"
