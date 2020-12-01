{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TypeFamilies       #-}

{-# LANGUAGE Safe               #-}

module Data.Tree.AVL.Extern.Constructors (
  AVL(AVL), mkAVL,
  IsAVLT(..), IsAVLC(..),
  IsAlmostAVLT(..)
) where

import           Data.Kind                (Type)
import           Data.Tree.Node           (Node)
import           Data.Tree.AVL.Invariants (BalancedHeights, Height)
import           Data.Tree.BST.Extern.Constructors (IsBSTT, IsBSTC(isBSTT))
import           Data.Tree.ITree          (ITree, Tree (EmptyTree,ForkTree))
import           Prelude                  (Show (show), Bool(True), (++), undefined)



-- | Constructor of AVLs. Given an arbitrary tree, it tests wether it verifies the BST and  AVL invariant.
data AVL :: Tree -> Type where
  AVL :: ITree t -> IsBSTT t -> IsAVLT t -> AVL t

instance Show (AVL t) where
  show (AVL t _ _) = "AVL $ " ++ show t


-- | Proof term which shows that `t` is an AVL
data IsAVLT :: Tree -> Type where
  EmptyIsAVLT :: IsAVLT 'EmptyTree
  ForkIsAVLT  :: (BalancedHeights (Height l) (Height r) ~ 'True) =>
    IsAVLT l -> Node n a -> IsAVLT r -> IsAVLT ('ForkTree l (Node n a) r)


-- | Class for constructing the proof term IsAVLT
class IsAVLC (t :: Tree) where
  isAVLT :: IsAVLT t

instance IsAVLC 'EmptyTree where
  isAVLT = EmptyIsAVLT
instance (IsAVLC l, IsAVLC r, BalancedHeights (Height l) (Height r) ~ 'True) =>
  IsAVLC ('ForkTree l (Node n a) r) where
  isAVLT = ForkIsAVLT isAVLT (undefined::Node n a) isAVLT


-- | Given an ITree, compute the proof terms IsBSTT and IsAVLT, in order to
-- | check if it is an AVL.
mkAVL :: (IsBSTC t, IsAVLC t) => ITree t -> AVL t
mkAVL t = AVL t isBSTT isAVLT


-- | Proof term which shows that `t` is an AlmostAVL
data IsAlmostAVLT :: Tree -> Type where
  ForkIsAlmostAVLT  :: IsAVLT l -> Node n a -> IsAVLT r -> IsAlmostAVLT ('ForkTree l (Node n a) r)

