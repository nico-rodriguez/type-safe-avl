{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Extern.BST (
  emptyBST,
  insertBST,
  lookupBST,
  deleteBST
) where

import           Data.Proxy
import           Data.Type.Equality
import           Extern.BSTOperations
import           Extern.BSTProofs
import           ITree
import           Node
import           Prelude              hiding (lookup)

emptyBST :: BST 'EmptyTree
emptyBST = BST EmptyITree

insertBST :: (Insertable x a t, ProofIsBSTInsert x a t) =>
  Proxy x -> a -> BST t -> BST (Insert x a t)
insertBST x a bst@(BST t) = gcastWith (proofIsBSTInsert node bst) BST $ insert node t
  where node = mkNode x a

lookupBST :: (t ~ 'ForkTree l (Node n a1) r, Member x t ~ 'True, Lookupable x a t) =>
  Proxy x -> BST t -> a
lookupBST p (BST t) = lookup p t

deleteBST :: (Deletable x t, ProofIsBSTDelete x t) =>
  Proxy x -> BST t -> BST (Delete x t)
deleteBST px bst@(BST t) = gcastWith (proofIsBSTDelete px bst) (BST $ delete px t)
