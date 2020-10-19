{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TypeFamilies       #-}

module Data.Tree.AVL.Extern.Constructor (
  AVL(AVL),
) where

import           Data.Kind                (Type)
import           Data.Tree.AVL.Invariants (IsAVL)
import           Data.Tree.BST.Invariants (IsBST)
import           Data.Tree.ITree          (ITree, Tree)
import           Prelude                  (Bool (True), Show (show), (++))



-- | Constructor of AVLs. Given an arbitrary tree, it tests wether it verifies the BST and  AVL invariant.
data AVL :: Tree -> Type where
  AVL :: (IsBST t ~ 'True, IsAVL t ~ 'True) => ITree t -> AVL t

instance Show (AVL t) where
  show (AVL t) = "AVL $ " ++ show t
