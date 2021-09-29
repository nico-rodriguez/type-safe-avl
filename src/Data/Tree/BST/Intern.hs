{-|
Module      : Data.Tree.BST.Intern
Description : Interface for internalist type safe BSTs
Copyright   : (c) Nicolás Rodríguez, 2021
License     : GPL-3
Maintainer  : Nicolás Rodríguez
Stability   : experimental
Portability : POSIX

Interface for the main functions over type safe BST trees
implemented with the internalist approach.
-}

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs              #-}

{-# LANGUAGE Safe               #-}

module Data.Tree.BST.Intern (
  emptyBST,
  insertBST,
  lookupBST,
  deleteBST
) where

import           Data.Proxy                       (Proxy)
import           Data.Tree.BST.Intern.Constructors (BST (EmptyBST))
import           Data.Tree.BST.Intern.Delete      (Deletable (Delete, delete))
import           Data.Tree.BST.Intern.Insert      (Insertable (Insert, insert))
import           Data.Tree.BST.Intern.Lookup      (Lookupable (lookup))
import           Data.Tree.BST.Utils              (Member)
import           Data.Tree.ITree                  (Tree (EmptyTree, ForkTree))
import           Data.Tree.Node                   (Node, mkNode)
import           Prelude                          (Bool (True))

-- | Empty BST tree with the internalist implementation.
emptyBST :: BST 'EmptyTree
emptyBST = EmptyBST

-- | Interface for the insertion algorithm in the internalist implementation.
insertBST :: (Insertable x a t) =>
  Proxy x -> a -> BST t -> BST (Insert x a t)
insertBST x a = insert node
  where node = mkNode x a

-- | Interface for the lookup algorithm in the internalist implementation.
lookupBST :: (t ~ 'ForkTree l (Node n a1) r, Member x t ~ 'True, Lookupable x a t) =>
  Proxy x -> BST t -> a
lookupBST = lookup

-- | Interface for the deletion algorithm in the internalist implementation.
deleteBST :: (Deletable x t) =>
  Proxy x -> BST t -> BST (Delete x t)
deleteBST = delete
