{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TypeFamilies       #-}

{-# LANGUAGE Safe               #-}

module Data.Tree.BST.Intern.Constructors (
  BST(EmptyBST,ForkBST),
) where

import           Data.Kind                (Type)
import           Data.Tree.BST.Invariants (GtN, LtN)
import           Data.Tree.ITree          (Tree (EmptyTree, ForkTree))
import           Data.Tree.Node           (Node)
import           Prelude                  (Bool (True), Show (show), String,
                                           (++))


-- | Constructor of BSTs. Given two BST trees and an arbitrary node,
-- | it tests wether the key of the node verifies the LtN and GtN invariants
-- | wtih respect to each tree.
-- | Notice that this is all that's needed to assert that the new tree is a BST.
data BST :: Tree -> Type where
  EmptyBST :: BST 'EmptyTree
  ForkBST  :: (Show a, LtN l n ~ 'True, GtN r n ~ 'True) =>
    BST l -> Node n a -> BST r -> BST ('ForkTree l (Node n a) r)

instance Show (BST t) where
  show EmptyBST         = "E"
  show (ForkBST l n r)  = "F " ++ go l ++ " " ++ show n ++ " " ++ go r
    where
      go :: BST t' -> String
      go EmptyBST         = "E"
      go (ForkBST l' n' r')  = "(F " ++ go l' ++ " " ++ show n' ++ " " ++ go r' ++ ")"
