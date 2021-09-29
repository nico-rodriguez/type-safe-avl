{-|
Module      : Data.Tree.BST.Extern
Description : Interface for externalist type safe BST trees
Copyright   : (c) Nicolás Rodríguez, 2021
License     : GPL-3
Maintainer  : Nicolás Rodríguez
Stability   : experimental
Portability : POSIX

Interface for the main functions over type safe BST trees
implemented with the externalist approach.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE Safe                  #-}

module Data.Tree.BST.Extern (
  emptyBST,
  insertBST,
  lookupBST,
  deleteBST
) where

import           Data.Proxy                        (Proxy)
import           Data.Tree.BST.Extern.Constructors (BST (BST), IsBSTT (EmptyIsBSTT))
import           Data.Tree.BST.Extern.Delete       (Deletable (Delete, delete))
import           Data.Tree.BST.Extern.DeleteProofs (ProofIsBSTDelete (proofIsBSTDelete))
import           Data.Tree.BST.Extern.Insert       (Insertable (Insert, insert))
import           Data.Tree.BST.Extern.InsertProofs (ProofIsBSTInsert (proofIsBSTInsert))
import           Data.Tree.BST.Extern.Lookup       (Lookupable (lookup))
import           Data.Tree.BST.Utils               (Member)
import           Data.Tree.ITree                   (ITree (EmptyITree),
                                                    Tree (EmptyTree, ForkTree))
import           Data.Tree.Node                    (Node, mkNode)
import           Prelude                           (Bool (True))

-- | Empty BST tree with the externalist implementation.
emptyBST :: BST 'EmptyTree
emptyBST = BST EmptyITree EmptyIsBSTT

-- | Interface for the insertion algorithm in the externalist implementation.
-- It calls @insert@ over `ITree` and @proofIsBSTInsert@ for constructing the evidence
-- that the new tree remains BST.
insertBST :: (Insertable x a t, ProofIsBSTInsert x a t) =>
  Proxy x -> a -> BST t -> BST (Insert x a t)
insertBST x a (BST t tIsBST) = BST (insert node t) (proofIsBSTInsert node tIsBST)
  where node = mkNode x a

-- | Interface for the lookup algorithm in the externalist implementation.
lookupBST :: (t ~ 'ForkTree l (Node n a1) r, Member x t ~ 'True, Lookupable x a t) =>
  Proxy x -> BST t -> a
lookupBST p (BST t _) = lookup p t

-- | Interface for the deletion algorithm in the externalist implementation.
-- It calls @delete@ over `ITree` and @proofIsBSTDelete@ for constructing the evidence
-- that the new tree remains BST.
deleteBST :: (Deletable x t, ProofIsBSTDelete x t) =>
  Proxy x -> BST t -> BST (Delete x t)
deleteBST px (BST t tIsBST) = BST (delete px t) (proofIsBSTDelete px tIsBST)
