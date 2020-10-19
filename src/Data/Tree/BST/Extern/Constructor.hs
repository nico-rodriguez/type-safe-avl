{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TypeFamilies       #-}

module Data.Tree.BST.Extern.Constructor (
  BST(BST)
) where

import           Data.Kind                (Type)
import           Data.Tree.BST.Invariants (IsBST)
import           Data.Tree.ITree          (ITree, Tree)
import           Prelude                  (Bool (True), Show (show), (++))


-- | Constructor of BSTs. Given an arbitrary tree, it tests wether it verifies the BST invariant.
data BST :: Tree -> Type where
  BST :: (IsBST t ~ 'True) => ITree t -> BST t

instance Show (BST t) where
  show (BST t) = "BST $ " ++ show t
