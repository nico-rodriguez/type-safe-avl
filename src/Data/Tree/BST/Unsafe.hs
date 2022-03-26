{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

-- |
-- Module      : Data.Tree.BST.Unsafe
-- Description : Unsafe BST trees
-- Copyright   : (c) Nicolás Rodríguez, 2021
-- License     : GPL-3
-- Maintainer  : Nicolás Rodríguez
-- Stability   : experimental
-- Portability : POSIX
--
-- Implementation of unsafe BST trees. These trees have no type level
-- information useful for compile time verification of invariants.
module Data.Tree.BST.Unsafe
  ( emptyBST,
    insertBST,
    lookupBST,
    deleteBST,
  )
where

import Data.Kind (Type)
import Prelude
  ( Int,
    Maybe (Just, Nothing),
    Ordering (EQ, GT, LT),
    Show,
    compare,
  )

-- | Nodes for unsafe `BST` trees. They only hold information
-- at the value level: some value of type @a@ and a key
-- of type `Int`.
data Node :: Type -> Type where
  Node :: Show a => Int -> a -> Node a

deriving stock instance Show (Node a)

-- | Constructor of unsafe `BST` trees.
data BST :: Type -> Type where
  E :: BST a
  F :: BST a -> Node a -> BST a -> BST a
  deriving stock (Show)

-- | Empty `BST` tree.
emptyBST :: BST a
emptyBST = E

-- | Entry point for inserting a new key and value.
-- If the key is already present in the tree, update the value.
insertBST :: Show a => Int -> a -> BST a -> BST a
insertBST x v E = F E (Node x v) E
insertBST x' v' t@(F _ (Node x _) _) = insertBST' (Node x' v') t (compare x' x)

-- | Insertion algorithm. It has the additional parameter of type
-- `Ordering`, which guides the recursion.
insertBST' :: Node a -> BST a -> Ordering -> BST a
insertBST' node (F l _ r) EQ = F l node r
insertBST' n' (F E n r) LT = F (F E n' E) n r
insertBST' n'@(Node x _) (F l@(F _ (Node ln _) _) n r) LT =
  F (insertBST' n' l (compare x ln)) n r
insertBST' n' (F l n E) GT = F l n (F E n' E)
insertBST' n'@(Node x _) (F l n r@(F _ (Node rn _) _)) GT =
  F l n (insertBST' n' r (compare x rn))

-- | Lookup the given key in the tree.
-- It returns `Nothing` if tree is empty or if it doesn't have the key.
lookupBST :: Int -> BST a -> Maybe a
lookupBST _ E = Nothing
lookupBST x t@(F _ (Node n _) _) = lookupBST' x t (compare x n)

-- | Lookup algorithm. It has the additional parameter of type
-- `Ordering`, which guides the recursion.
lookupBST' :: Int -> BST a -> Ordering -> Maybe a
lookupBST' _ E _ = Nothing
lookupBST' _ (F _ (Node _ a) _) EQ = Just a
lookupBST' _ (F E _ _) LT = Nothing
lookupBST' _ (F _ _ E) GT = Nothing
lookupBST' x (F l@(F _ (Node ln _) _) _ _) LT = lookupBST' x l (compare x ln)
lookupBST' x (F _ _ r@(F _ (Node rn _) _)) GT = lookupBST' x r (compare x rn)

-- | Delete the node with the maximum key value.
maxKeyDelete :: BST a -> BST a
maxKeyDelete E = E
maxKeyDelete (F l _ E) = l
maxKeyDelete (F l node r@F {}) =
  F l node (maxKeyDelete r)

-- | Get the node with maximum key value.
-- | It returns `Nothing` if tree is empty.
maxNode :: BST a -> Maybe (Node a)
maxNode E = Nothing
maxNode (F _ node E) = Just node
maxNode (F _ (Node _ _) r@F {}) = maxNode r

-- | Delete the node with the given key.
-- If the key is not in the tree, return the same tree.
deleteBST :: Int -> BST a -> BST a
deleteBST _ E = E
deleteBST x t@(F _ (Node n _) _) = deleteBST' x t (compare x n)

-- | Deletion algorithm. It has the additional parameter of type
-- `Ordering`, which guides the recursion.
deleteBST' :: Int -> BST a -> Ordering -> BST a
deleteBST' _ (F E _ E) EQ = E
deleteBST' _ (F E _ r@F {}) EQ = r
deleteBST' _ (F l@F {} _ E) EQ = l
deleteBST' _ (F l@F {} _ r@F {}) EQ =
  F (maxKeyDelete l) mNode r
  where
    Just mNode = maxNode l
deleteBST' _ t@(F E _ _) LT = t
deleteBST' x (F l@(F _ (Node ln _) _) node r) LT =
  F (deleteBST' x l (compare x ln)) node r
deleteBST' _ t@(F _ _ E) GT = t
deleteBST' x (F l node r@(F _ (Node rn _) _)) GT =
  F l node (deleteBST' x r (compare x rn))
