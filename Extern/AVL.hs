{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe                  #-}

module Extern.AVL (
  AVL(..),
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
import           ITree                (Tree (ForkTree))
import           Node                 (Node)
import           Prelude              (Bool (True), ($))

insertAVL :: (Insertable x a t, ProofIsBSTInsert x a t, ProofIsAVLInsert x a t) =>
  Node x a -> AVL t -> AVL (Insert x a t)
insertAVL x (AVL t) = gcastWith (proofIsAVLInsert x t) $ gcastWith (proofIsBSTInsert x t) AVL $ insert x t

lookupAVL :: (t ~ 'ForkTree l (Node n a1) r, Member x t ~ 'True, Lookupable x a t) =>
  Proxy x -> AVL t -> a
lookupAVL p (AVL t) = lookup p t

deleteAVL :: (Deletable x t, ProofIsBSTDelete x t, ProofIsAVLDelete x t) =>
  Proxy x -> AVL t -> AVL (Delete x t)
deleteAVL px (AVL t) = gcastWith (proofIsAVLDelete px t) $ gcastWith (proofIsBSTDelete px t) (AVL $ delete px t)
