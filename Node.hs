{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE Safe               #-}
{-# LANGUAGE StandaloneDeriving #-}

module Node where

import           Data.Kind    (Type)
import           Data.Proxy   (Proxy (Proxy))
import           GHC.TypeLits (Nat)
import           Prelude      (Char, Show)

data Node :: Nat -> Type -> Type where
  Node :: a -> Node k a
deriving instance Show a => Show (Node k a)

mkNode :: forall (k::Nat)(a::Type). Proxy k -> a -> Node k a
mkNode _ = Node

getValue :: forall (k::Nat)(a::Type). Node k a -> a
getValue (Node a) = a

nod :: Node 1 Char
nod = mkNode (Proxy::Proxy 1) 'c'

val :: Char
val = getValue nod
