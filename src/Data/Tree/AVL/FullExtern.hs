{-# LANGUAGE Safe #-}

-- |
-- Module      : Data.Tree.AVL.FullExtern
-- Description : Interface for fully externalist type safe AVL trees
-- Copyright   : (c) Nicolás Rodríguez, 2021
-- License     : GPL-3
-- Maintainer  : Nicolás Rodríguez
-- Stability   : experimental
-- Portability : POSIX
--
-- Interface for the main functions over type safe AVL trees
-- implemented with the internalist approach. This module reexports
-- the functions defined over `ITree` trees from the modules
-- "Data.Tree.AVL.Extern.Constructors", "Data.Tree.AVL.Extern.Delete",
-- "Data.Tree.AVL.Extern.Insert" and "Data.Tree.AVL.Extern.Lookup".
module Data.Tree.AVL.FullExtern
  ( AVL (AVL),
    mkAVL,
    ITree (EmptyITree),
    insert,
    lookup,
    delete,
  )
where

import Data.Tree.AVL.Extern.Constructors (AVL (AVL), mkAVL)
import Data.Tree.AVL.Extern.Delete (Deletable (delete))
import Data.Tree.AVL.Extern.Insert (Insertable (insert))
import Data.Tree.BST.Extern.Constructors ()
import Data.Tree.BST.Extern.Lookup (Lookupable (lookup))
import Data.Tree.ITree (ITree (EmptyITree))
import Prelude ()
