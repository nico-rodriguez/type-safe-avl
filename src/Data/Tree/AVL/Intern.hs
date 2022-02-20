{-|
Module      : Data.Tree.AVL.Intern
Description : Interface for internalist type safe AVL trees
Copyright   : (c) Nicolás Rodríguez, 2021
License     : GPL-3
Maintainer  : Nicolás Rodríguez
Stability   : experimental
Portability : POSIX

Interface for the main functions over type safe AVL trees
implemented with the internalist approach.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}

{-# LANGUAGE Safe                  #-}

module Data.Tree.AVL.Intern (
  emptyAVL,
  insertAVL,
  lookupAVL,
  deleteAVL
) where

import           Data.Proxy                       (Proxy)
import           Data.Tree.AVL.Intern.Constructors (AVL (EmptyAVL))
import           Data.Tree.AVL.Intern.Delete      (Deletable (Delete, delete))
import           Data.Tree.AVL.Intern.Insert      (Insertable (Insert, insert))
import           Data.Tree.AVL.Intern.Lookup      (Lookupable (lookup))
import           Data.Tree.BST.Utils              (Member)
import           Data.Tree.ITree                  (Tree (EmptyTree, ForkTree))
import           Data.Tree.Node                   (Node, mkNode)
import           Prelude                          (Bool (True))


-- | Empty `AVL` tree with the internalist implementation.
emptyAVL :: AVL 'EmptyTree
emptyAVL = EmptyAVL

-- | Interface for the insertion algorithm in the internalist implementation.
-- It calls `insert` over an internalist `AVL` tree.
insertAVL :: (Insertable x a t) =>
  Proxy x -> a -> AVL t -> AVL (Insert x a t)
insertAVL x a = insert node
  where node = mkNode x a

-- | Interface for the lookup algorithm in the internalist implementation for `AVL`.
lookupAVL :: (t ~ 'ForkTree l (Node n a1) r, Member x t t ~ 'True, Lookupable x a t) =>
  Proxy x -> AVL t -> a
lookupAVL = lookup

-- | Interface for the deletion algorithm in the internalist implementation.
-- It calls `delete` over an internalist `AVL` tree.
deleteAVL :: (Deletable x t) =>
  Proxy x -> AVL t -> AVL (Delete x t)
deleteAVL = delete
