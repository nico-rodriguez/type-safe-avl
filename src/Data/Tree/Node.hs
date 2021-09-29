{-|
Module      : Data.Tree.Node
Description : Nodes for type safe trees
Copyright   : (c) Nicolás Rodríguez, 2021
License     : GPL-3
Maintainer  : Nicolás Rodríguez
Stability   : experimental
Portability : POSIX

Definition of nodes for the type safe trees, along with a constructor and a value getter.
-}

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE Safe               #-}

module Data.Tree.Node (Node, mkNode, getValue) where

import           Data.Kind    (Type)
import           Data.Proxy   (Proxy)
import           GHC.TypeLits (Nat)
import           Prelude      (Show)

-- | Node constructor. It receives a value of an arbitrary type (of kind 'Type')
-- and returns a new `Node`. At the type level, it has the type `a` of the value
-- and a key `k` of kind `Nat`.
-- A key may be defined through explicit type signature, like @Node 'a' :: Node 6 Char@.
data Node :: Nat -> Type -> Type where
  Node :: a -> Node k a
deriving stock instance Show a => Show (Node k a)

-- | Node constructor. Besides the value to be stored, it receives the key
-- through a `Proxy`. It may be used like @node = mkNode (Proxy::Proxy 6) 'a'@.
mkNode :: Proxy k -> a -> Node k a
mkNode _ = Node

-- | Value getter from a `Node`.
getValue :: Node k a -> a
getValue (Node a) = a
