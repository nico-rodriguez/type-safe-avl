{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Intern.BST (
  emptyBST,
  insertBST,
  lookupBST,
  deleteBST
) where

import           Data.Proxy (Proxy)
import           Intern.BSTOperations (BST(EmptyBST), Insertable(Insert,insert),
                                      Member, Lookupable(lookup),
                                      Deletable(Delete,delete))
import           ITree                 (Tree(EmptyTree,ForkTree))
import           Node  (Node, mkNode)
import           Prelude              hiding (lookup)

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
