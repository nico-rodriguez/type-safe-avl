{-|
Module      : Data.Tree.BST.FullExtern
Description : Interface for fully externalist type safe BSTs
Copyright   : (c) Nicolás Rodríguez, 2021
License     : GPL-3
Maintainer  : Nicolás Rodríguez
Stability   : experimental
Portability : POSIX

Interface for the main functions over type safe BST trees
implemented with the internalist approach. This module reexports
the functions defined over `ITree` trees from the modules
"Data.Tree.BST.Extern.Constructors", "Data.Tree.BST.Extern.Delete", 
"Data.Tree.BST.Extern.Insert" and "Data.Tree.BST.Extern.Lookup".
-}

{-# LANGUAGE Safe                  #-}

module Data.Tree.BST.FullExtern (
  BST (BST), mkBST,
  ITree (EmptyITree),
  insert, lookup, delete
) where

import           Data.Tree.BST.Extern.Constructors (BST (BST), mkBST)
import           Data.Tree.BST.Extern.Delete      (Deletable (delete))
import           Data.Tree.BST.Extern.Insert      (Insertable (insert))
import           Data.Tree.BST.Extern.Lookup      (Lookupable (lookup))
import           Data.Tree.ITree                  (ITree (EmptyITree))
import           Prelude                          ()
