{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE Safe                  #-}

module Data.Tree.AVL.Extern (
  emptyAVL,
  insertAVL,
  lookupAVL,
  deleteAVL
) where

import           Data.Proxy                        (Proxy)
import           Data.Tree.AVL.Extern.Constructors (AVL (AVL), IsAVLT (EmptyIsAVLT))
import           Data.Tree.AVL.Extern.Delete       (Deletable (Delete, delete))
import           Data.Tree.AVL.Extern.DeleteProofs (ProofIsAVLDelete (proofIsAVLDelete),
                                                    ProofIsBSTDelete (proofIsBSTDelete))
import           Data.Tree.AVL.Extern.Insert       (Insertable (Insert, insert))
import           Data.Tree.AVL.Extern.InsertProofs (ProofIsAVLInsert (proofIsAVLInsert),
                                                    ProofIsBSTInsert (proofIsBSTInsert))
import           Data.Tree.BST.Extern.Constructors (IsBSTT (EmptyIsBSTT))
import           Data.Tree.BST.Extern.Lookup       (Lookupable (lookup))
import           Data.Tree.BST.Utils               (Member)
import           Data.Tree.ITree                   (ITree (EmptyITree),
                                                    Tree (EmptyTree, ForkTree))
import           Data.Tree.Node                    (Node, mkNode)
import           Prelude                           (Bool (True))


emptyAVL :: AVL 'EmptyTree
emptyAVL = AVL EmptyITree EmptyIsBSTT EmptyIsAVLT

insertAVL :: (Insertable x a t, ProofIsBSTInsert x a t, ProofIsAVLInsert x a t) =>
  Proxy x -> a -> AVL t -> AVL (Insert x a t)
insertAVL x a (AVL t tIsBST tIsAVL) = AVL (insert node t) (proofIsBSTInsert node tIsBST) (proofIsAVLInsert node tIsAVL)
  where node = mkNode x a

lookupAVL :: (t ~ 'ForkTree l (Node n a1) r, Member x t ~ 'True, Lookupable x a t) =>
  Proxy x -> AVL t -> a
lookupAVL p (AVL t _ _) = lookup p t

deleteAVL :: (Deletable x t, ProofIsBSTDelete x t, ProofIsAVLDelete x t) =>
  Proxy x -> AVL t -> AVL (Delete x t)
deleteAVL px (AVL t tIsBST tIsAVL) = AVL (delete px t) (proofIsBSTDelete px tIsBST) (proofIsAVLDelete px tIsAVL)
