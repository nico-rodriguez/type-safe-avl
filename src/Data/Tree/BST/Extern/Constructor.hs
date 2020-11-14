{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TypeFamilies       #-}

module Data.Tree.BST.Extern.Constructor (
  BST(BST), mkBST
) where

import           Data.Kind                (Type)
import           Data.Tree.BST.Invariants (IsBSTT, IsBSTC(..))
import           Data.Tree.ITree          (ITree, Tree)
import           Prelude                  (Show (show), (++))


-- | Constructor of BSTs. Given an arbitrary tree, it tests wether it verifies the BST invariant.
data BST :: Tree -> Type where
  BST :: ITree t -> IsBSTT t -> BST t

instance Show (BST t) where
  show (BST t _) = "BST $ " ++ show t


-- | Given an ITree, compute the proof term IsBSTT, in order to
-- | check if it is a BST.
mkBST :: (IsBSTC t) => ITree t -> BST t
mkBST t = BST t isBSTT
