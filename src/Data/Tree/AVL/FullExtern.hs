{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Tree.AVL.FullExtern (
  AVL (AVL),
  ITree (EmptyITree),
  insert, lookup, delete
) where

import           Data.Tree.AVL.Extern.Constructor (AVL (AVL))
import           Data.Tree.AVL.Extern.Delete      (Deletable (delete))
import           Data.Tree.AVL.Extern.Insert      (Insertable (insert))
import           Data.Tree.BST.Extern.Lookup      (Lookupable (lookup))
import           Data.Tree.ITree                  (ITree (EmptyITree))
import           Prelude                          ()
