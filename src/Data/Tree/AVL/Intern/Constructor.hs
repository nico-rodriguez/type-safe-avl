{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TypeFamilies       #-}

{-# LANGUAGE Safe               #-}

module Data.Tree.AVL.Intern.Constructor (
  AVL(EmptyAVL,ForkAVL),
  AlmostAVL(NullAVL, AlmostAVL)
) where

import           Data.Kind                (Type)
import           Data.Tree.AVL.Invariants (BalancedHeights, Height)
import           Data.Tree.BST.Invariants (GtN, LtN)
import           Data.Tree.ITree          (Tree (EmptyTree, ForkTree))
import           Data.Tree.Node           (Node (Node))
import           Prelude                  (Bool (True), Show (show), String,
                                           (++))


-- | Constructor of AVLs. Given two AVL trees and an arbitrary node,
-- | it tests wether the key of the node verifies the LtN and GtN invariants
-- | wtih respect to each tree and if the heights are balanced.
-- | Notice that this is all that's needed to assert that the new tree is a AVL.
data AVL :: Tree -> Type where
  EmptyAVL :: AVL 'EmptyTree
  ForkAVL  :: (Show a, LtN l n ~ 'True, GtN r n ~ 'True, BalancedHeights (Height l) (Height r) ~ 'True) =>
    AVL l -> Node n a -> AVL r -> AVL ('ForkTree l (Node n a) r)

instance Show (AVL t) where
  show EmptyAVL         = "E"
  show (ForkAVL l n@(Node _) r)  = "F " ++ go l ++ " " ++ show n ++ " " ++ go r
    where
      go :: AVL t' -> String
      go EmptyAVL         = "E"
      go (ForkAVL l' n'@(Node _) r')  = "(F " ++ go l' ++ " " ++ show n' ++ " " ++ go r' ++ ")"


-- | Constructor of AlmostAVLs. This kind of trees arises after
-- | an insertion or deletion over an AVL that may leave
-- | the tree unbalanced.
data AlmostAVL :: Tree -> Type where
  NullAVL    :: AlmostAVL 'EmptyTree
  AlmostAVL  :: (Show a, LtN l n ~ 'True, GtN r n ~ 'True) =>
    AVL l -> Node n a -> AVL r -> AlmostAVL ('ForkTree l (Node n a) r)
