{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Tree.AVL.Intern.Constructor
-- Description : Constructor of type safe internalist AVL trees
-- Copyright   : (c) Nicolás Rodríguez, 2021
-- License     : GPL-3
-- Maintainer  : Nicolás Rodríguez
-- Stability   : experimental
-- Portability : POSIX
--
-- Implementation of the constructor of type safe internalist AVL
-- trees and instance definition for the `Show` type class.
module Data.Tree.AVL.Intern.Constructors
  ( AVL (EmptyAVL, ForkAVL),
    AlmostAVL (AlmostAVL),
  )
where

import Data.Kind (Type)
import Data.Tree.AVL.Invariants (BalancedHeights, Height)
import Data.Tree.BST.Invariants (GtN, LtN)
import Data.Tree.ITree (Tree (EmptyTree, ForkTree))
import Data.Tree.Node (Node)
import Prelude
  ( Bool (True),
    Show (show),
    String,
    (++),
  )

-- | Constructor of `AVL` trees. Given two `AVL` trees and an arbitrary node,
-- it tests at compile time wether the key of the node verifies the `LtN` and `GtN` invariants
-- with respect to each tree and if the heights are balanced.
-- Notice that, by inductive reasoning, this is all that's needed to assert that the new tree is a `AVL`.
data AVL :: Tree -> Type where
  EmptyAVL :: AVL 'EmptyTree
  ForkAVL ::
    (Show a, LtN l n ~ 'True, GtN r n ~ 'True, BalancedHeights (Height l) (Height r) n ~ 'True) =>
    AVL l ->
    Node n a ->
    AVL r ->
    AVL ('ForkTree l (Node n a) r)

-- | Instance definition for the `Show` type class.
instance Show (AVL t) where
  show EmptyAVL = "E"
  show (ForkAVL l n r) = "F " ++ go l ++ " " ++ show n ++ " " ++ go r
    where
      go :: AVL t' -> String
      go EmptyAVL = "E"
      go (ForkAVL l' n' r') = "(F " ++ go l' ++ " " ++ show n' ++ " " ++ go r' ++ ")"

-- | Constructor of `AlmostAVL` tress. This kind of trees arises after
-- an insertion or deletion over an `AVL` that may leave
-- the tree unbalanced.
data AlmostAVL :: Tree -> Type where
  AlmostAVL ::
    (Show a, LtN l n ~ 'True, GtN r n ~ 'True) =>
    AVL l ->
    Node n a ->
    AVL r ->
    AlmostAVL ('ForkTree l (Node n a) r)
