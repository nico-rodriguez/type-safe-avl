{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Tree.BST.Extern (
  emptyBST,
  insertBST,
  lookupBST,
  deleteBST
) where

import           Data.Proxy           (Proxy)
import           Data.Type.Equality   (gcastWith)
import           Data.Tree.BST.Extern.Operations (Deletable (Delete, delete),
                                       Insertable (Insert, insert),
                                       Lookupable (lookup), Member)
import           Data.Tree.BST.Extern.Proofs     (BST (BST),
                                       ProofIsBSTDelete (proofIsBSTDelete),
                                       ProofIsBSTInsert (proofIsBSTInsert))
import           Data.Tree.ITree                (ITree (EmptyITree),
                                       Tree (EmptyTree, ForkTree))
import           Data.Tree.Node                 (Node, mkNode)
import           Prelude              (Bool (True), ($))

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
