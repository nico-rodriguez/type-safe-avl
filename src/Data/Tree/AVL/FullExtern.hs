{-|
Module      : W
Description : 
Copyright   : (c) Nicolás Rodríguez, 2021
License     : GPL-3
Maintainer  : Nicolás Rodríguez
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

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
