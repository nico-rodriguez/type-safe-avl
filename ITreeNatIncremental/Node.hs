{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}

module ITreeNatIncremental.Node where

import           Data.Kind
import           Data.Proxy
import           GHC.TypeLits

data Node :: k -> Type -> Type where
  Node :: forall (k :: Nat) (a :: Type). a -> Node k a
deriving instance Show a => Show (Node k a)

mkNode :: forall (k::Nat)(a::Type). Proxy k -> a -> Node k a
mkNode _ = Node

getValue :: forall (k::Nat)(a::Type). Node k a -> a
getValue (Node a) = a

nod :: Node 1 Char
nod = mkNode (Proxy::Proxy 1) 'c'

val :: Char
val = getValue nod
