{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}

module Extern.AVL (
  AVL(..),
  insertAVL,
  lookupAVL,
  deleteAVL
) where

import           Data.Proxy
import           Data.Type.Equality
import           Extern.AVLOperations
import           Extern.AVLProofs
import           Extern.BSTOperations (Lookupable (..), Member)
import           ITree
import           Node
import           Prelude              hiding (lookup)

insertAVL :: (Insertable x a t, ProofIsBSTInsert x a t, ProofIsAVLInsert x a t) =>
  Node x a -> AVL t -> AVL (Insert x a t)
insertAVL x (AVL t) = gcastWith (proofIsAVLInsert x t) $ gcastWith (proofIsBSTInsert x t) AVL $ insert x t

lookupAVL :: (t ~ 'ForkTree l (Node n a1) r, Member x t ~ 'True, Lookupable x a t) =>
  Proxy x -> AVL t -> a
lookupAVL p (AVL t) = lookup p t

deleteAVL :: (Deletable x t, ProofIsBSTDelete x t, ProofIsAVLDelete x t) =>
  Proxy x -> AVL t -> AVL (Delete x t)
deleteAVL px (AVL t) = gcastWith (proofIsAVLDelete px t) $ gcastWith (proofIsBSTDelete px t) (AVL $ delete px t)
