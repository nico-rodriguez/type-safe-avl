{-# LANGUAGE Safe                  #-}

module Data.Tree.AVL.FullExtern (
  AVL (AVL), mkAVL,
  ITree (EmptyITree),
  insert, lookup, delete
) where

import           Data.Tree.AVL.Extern.Constructors (AVL (AVL), mkAVL)
import           Data.Tree.BST.Extern.Constructors ()
import           Data.Tree.AVL.Extern.Delete      (Deletable (delete))
import           Data.Tree.AVL.Extern.Insert      (Insertable (insert))
import           Data.Tree.BST.Extern.Lookup      (Lookupable (lookup))
import           Data.Tree.ITree                  (ITree (EmptyITree))
import           Prelude                          ()
