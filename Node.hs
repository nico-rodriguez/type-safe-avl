{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE Safe #-}

module Node where

import           Prelude (Show, Char)
import           Data.Kind (Type)
import           Data.Proxy (Proxy(Proxy))
import           GHC.TypeLits (Nat)

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
