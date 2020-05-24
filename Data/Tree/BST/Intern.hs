{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Tree.BST.Intern (
  emptyBST,
  insertBST,
  lookupBST,
  deleteBST
) where

import           Data.Proxy                      (Proxy)
import           Data.Tree.BST.Intern.Operations (BST (EmptyBST),
                                                  Deletable (Delete, delete),
                                                  Insertable (Insert, insert),
                                                  Lookupable (lookup), Member)
import           Data.Tree.ITree                 (Tree (EmptyTree, ForkTree))
import           Data.Tree.Node                  (Node, mkNode)
import           Prelude                         (Bool (True))

emptyBST :: BST 'EmptyTree
emptyBST = EmptyBST

insertBST :: (Insertable x a t) =>
  Proxy x -> a -> BST t -> BST (Insert x a t)
insertBST x a = insert node
  where node = mkNode x a

lookupBST :: (t ~ 'ForkTree l (Node n a1) r, Member x t ~ 'True, Lookupable x a t) =>
  Proxy x -> BST t -> a
lookupBST = lookup

deleteBST :: (Deletable x t) =>
  Proxy x -> BST t -> BST (Delete x t)
deleteBST = delete
