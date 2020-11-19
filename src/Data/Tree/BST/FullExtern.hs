{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

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
