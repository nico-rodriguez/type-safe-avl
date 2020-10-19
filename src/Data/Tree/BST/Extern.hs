{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Tree.BST.Extern (
  emptyBST,
  insertBST,
  lookupBST,
  deleteBST
) where

import           Data.Proxy                        (Proxy)
import           Data.Tree.BST.Extern.Constructor  (BST (BST))
import           Data.Tree.BST.Extern.Delete       (Deletable (Delete, delete))
import           Data.Tree.BST.Extern.DeleteProofs (ProofIsBSTDelete (proofIsBSTDelete))
import           Data.Tree.BST.Extern.Insert       (Insertable (Insert, insert))
import           Data.Tree.BST.Extern.InsertProofs (ProofIsBSTInsert (proofIsBSTInsert))
import           Data.Tree.BST.Extern.Lookup       (Lookupable (lookup))
import           Data.Tree.BST.Utils               (Member)
import           Data.Tree.ITree                   (ITree (EmptyITree),
                                                    Tree (EmptyTree, ForkTree))
import           Data.Tree.Node                    (Node, mkNode)
import           Data.Type.Equality                (gcastWith)
import           Prelude                           (Bool (True), ($))

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
