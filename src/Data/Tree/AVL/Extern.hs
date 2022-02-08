{-|
Module      : Data.Tree.AVL.Extern
Description : Interface for externalist type safe AVL trees
Copyright   : (c) Nicolás Rodríguez, 2021
License     : GPL-3
Maintainer  : Nicolás Rodríguez
Stability   : experimental
Portability : POSIX

Interface for the main functions over type safe AVL trees
implemented with the externalist approach.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# LANGUAGE Safe                  #-}

module Data.Tree.AVL.Extern (
  emptyAVL,
  insertAVL,
  lookupAVL,
  deleteAVL
) where

import           Data.Proxy                        (Proxy (Proxy))
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

-- | Empty `AVL` tree with the externalist implementation.
emptyAVL :: AVL 'EmptyTree
emptyAVL = AVL EmptyITree EmptyIsBSTT EmptyIsAVLT

-- | Interface for the insertion algorithm in the externalist implementation.
-- It calls `insert` over `ITree`, and `proofIsBSTInsert` and `proofIsAVLInsert` for constructing the evidence
-- that the new tree remains `AVL`.
insertAVL :: (Insertable x a t, ProofIsBSTInsert x a t, ProofIsAVLInsert x a t) =>
  Proxy x -> a -> AVL t -> AVL (Insert x a t)
insertAVL (px :: Proxy x) (a :: a) (AVL t tIsBST tIsAVL) = AVL (insert node t) (proofIsBSTInsert pNode tIsBST) (proofIsAVLInsert pNode tIsAVL)
  where node  = mkNode px a
        pNode = Proxy :: Proxy (Node x a)

-- | Interface for the lookup algorithm in the externalist implementation of `AVL`.
lookupAVL :: (t ~ 'ForkTree l (Node n a1) r, Member x t t ~ 'True, Lookupable x a t) =>
  Proxy x -> AVL t -> a
lookupAVL p (AVL t _ _) = lookup p t

-- | Interface for the deletion algorithm in the externalist implementation.
-- It calls `delete` over `ITree`, and `proofIsBSTDelete` and `proofIsAVLDelete`  for constructing the evidence
-- that the new tree remains `AVL`.
deleteAVL :: (Deletable x t, ProofIsBSTDelete x t, ProofIsAVLDelete x t) =>
  Proxy x -> AVL t -> AVL (Delete x t)
deleteAVL px (AVL t tIsBST tIsAVL) = AVL (delete px t) (proofIsBSTDelete px tIsBST) (proofIsAVLDelete px tIsAVL)
