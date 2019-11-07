{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}

module Intern.BST (
  BST(..),
  insertBST,
  lookupBST,
  deleteBST
) where

import           Data.Proxy
import           Intern.BSTOperations
import           ITree
import           Node
import           Prelude              hiding (lookup)

insertBST :: (Insertable x a t) =>
  Node x a -> BST t -> BST (Insert x a t)
insertBST = insert

lookupBST :: (t ~ 'ForkTree l (Node n a1) r, Member x t ~ 'True, Lookupable x a t) =>
  Proxy x -> BST t -> a
lookupBST = lookup

deleteBST :: (Deletable x t) =>
  Proxy x -> BST t -> BST (Delete x t)
deleteBST = delete
