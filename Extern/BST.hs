{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}

module Extern.BST (
  BST(..),
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

insertBST :: (Insertable x a t, ProofIsBSTInsert x a t) =>
  Node x a -> BST t -> BST (Insert x a t)
insertBST x (BST t) = gcastWith (proofIsBSTInsert x t) BST $ insert x t

lookupBST :: (t ~ 'ForkTree l (Node n a1) r, Member x t ~ 'True, Lookupable x a t) =>
  Proxy x -> BST t -> a
lookupBST p (BST t) = lookup p t

deleteBST :: (Deletable x t, ProofIsBSTDelete x t) =>
  Proxy x -> BST t -> BST (Delete x t)
deleteBST px (BST t) = gcastWith (proofIsBSTDelete px t) (BST $ delete px t)
