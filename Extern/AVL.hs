{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Extern.AVL (
  emptyAVL,
  insertAVL,
  lookupAVL,
  deleteAVL
) where

import           Data.Proxy           (Proxy)
import           Data.Type.Equality   (gcastWith)
import           Extern.AVLOperations (Deletable (Delete, delete),
                                       Insertable (Insert, insert))
import           Extern.AVLProofs     (AVL (AVL),
                                       ProofIsAVLDelete (proofIsAVLDelete),
                                       ProofIsAVLInsert (proofIsAVLInsert),
                                       ProofIsBSTDelete (proofIsBSTDelete),
                                       ProofIsBSTInsert (proofIsBSTInsert))
import           Extern.BSTOperations (Lookupable (lookup), Member)
import           Extern.BSTProofs     (BST (BST))
import           ITree                (ITree (EmptyITree),
                                       Tree (EmptyTree, ForkTree))
import           Node                 (Node, mkNode)
import           Prelude              (Bool (True), ($))


emptyAVL :: AVL 'EmptyTree
emptyAVL = AVL EmptyITree

insertAVL :: (Insertable x a t, ProofIsBSTInsert x a t, ProofIsAVLInsert x a t) =>
  Proxy x -> a -> AVL t -> AVL (Insert x a t)
insertAVL x a avl@(AVL t) = gcastWith (proofIsAVLInsert node avl) $ gcastWith (proofIsBSTInsert node (BST t)) AVL $ insert node t
  where node = mkNode x a

lookupAVL :: (t ~ 'ForkTree l (Node n a1) r, Member x t ~ 'True, Lookupable x a t) =>
  Proxy x -> AVL t -> a
lookupAVL p (AVL t) = lookup p t

deleteAVL :: (Deletable x t, ProofIsBSTDelete x t, ProofIsAVLDelete x t) =>
  Proxy x -> AVL t -> AVL (Delete x t)
deleteAVL px avl@(AVL t) = gcastWith (proofIsAVLDelete px avl) $ gcastWith (proofIsBSTDelete px (BST t)) (AVL $ delete px t)
