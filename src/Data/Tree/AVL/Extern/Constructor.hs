{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TypeFamilies       #-}

{-# LANGUAGE Safe                  #-}

module Data.Tree.AVL.Extern.Constructor (
  AVL(AVL), mkAVL
) where

import           Data.Kind                (Type)
import           Data.Tree.AVL.Invariants (IsAVLT, IsAVLC(isAVLT))
import           Data.Tree.BST.Invariants (IsBSTT, IsBSTC(isBSTT))
import           Data.Tree.ITree          (ITree, Tree)
import           Prelude                  (Show (show), (++))



-- | Constructor of AVLs. Given an arbitrary tree, it tests wether it verifies the BST and  AVL invariant.
data AVL :: Tree -> Type where
  AVL :: ITree t -> IsBSTT t -> IsAVLT t -> AVL t

instance Show (AVL t) where
  show (AVL t _ _) = "AVL $ " ++ show t

-- | Given an ITree, compute the proof terms IsBSTT and IsAVLT, in order to
-- | check if it is an AVL.
mkAVL :: (IsBSTC t, IsAVLC t) => ITree t -> AVL t
mkAVL t = AVL t isBSTT isAVLT
