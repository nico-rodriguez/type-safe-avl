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

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TypeFamilies       #-}

{-# LANGUAGE Safe               #-}

module Data.Tree.BST.Extern.Constructors (
  BST(BST), mkBST,
  IsBSTT(..), IsBSTC(..)
) where

import           Data.Kind                (Type)
import           Data.Tree.Node           (Node)
import           Data.Tree.BST.Invariants (LtN, GtN)
import           Data.Tree.ITree          (ITree, Tree(EmptyTree,ForkTree))
import           Prelude                  (Show (show), Bool(True), (++), undefined)


-- | Constructor of BSTs. Given an arbitrary tree, it tests wether it verifies the BST invariant.
data BST :: Tree -> Type where
  BST :: ITree t -> IsBSTT t -> BST t

instance Show (BST t) where
  show (BST t _) = "BST $ " ++ show t



-- | Proof term which shows that `t` is a BST
data IsBSTT :: Tree -> Type where
  EmptyIsBSTT :: IsBSTT 'EmptyTree
  ForkIsBSTT  :: (LtN l n ~ 'True, GtN r n ~ 'True) =>
    IsBSTT l -> Node n a -> IsBSTT r -> IsBSTT ('ForkTree l (Node n a) r)

-- | Class for constructing the proof term IsBSTT
class IsBSTC (t :: Tree) where
  isBSTT :: IsBSTT t

instance IsBSTC 'EmptyTree where
  isBSTT = EmptyIsBSTT
instance (IsBSTC l, IsBSTC r, LtN l n ~ 'True, GtN r n ~ 'True) =>
  IsBSTC ('ForkTree l (Node n a) r) where
  isBSTT = ForkIsBSTT isBSTT (undefined::Node n a) isBSTT


-- | Given an ITree, compute the proof term IsBSTT, in order to
-- | check if it is a BST.
mkBST :: (IsBSTC t) => ITree t -> BST t
mkBST t = BST t isBSTT
