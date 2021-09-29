{-# OPTIONS_HADDOCK ignore-exports #-}

{-|
Module      : Data.Tree.BST.Extern.Lookup
Description : Lookup algorithm over ITree trees
Copyright   : (c) Nicolás Rodríguez, 2021
License     : GPL-3
Maintainer  : Nicolás Rodríguez
Stability   : experimental
Portability : POSIX

Implementation of the lookup algorithm over ITree trees for
externalist BST trees.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# LANGUAGE Safe                  #-}

module Data.Tree.BST.Extern.Lookup (
  Lookupable(lookup)
) where

import           Data.Kind           (Type)
import           Data.Proxy          (Proxy (Proxy))
import           Data.Tree.BST.Utils (LookupValueType, Member)
import           Data.Tree.ITree     (ITree (ForkITree), Tree (ForkTree))
import           Data.Tree.Node      (Node, getValue)
import           GHC.TypeNats        (CmpNat, Nat)
import           Prelude             (Bool (True), Ordering (EQ, GT, LT))


-- | This type class provides the functionality to lookup a node with key 'x'
-- in a non empty tree 't' without checking any structural invariant (key ordering).
-- The lookup is defined at the value level and the type level, and is performed
-- as if the tree is a `BST`.
-- It's necessary to know the type 'a' of the value stored in node with key 'x'
-- so that the type of the value returned by 'lookup' may be specified.
class Lookupable (x :: Nat) (a :: Type) (t :: Tree) where
  lookup :: (t ~ 'ForkTree l (Node n a1) r, Member x t ~ 'True) =>
    Proxy x -> ITree t -> a
instance (a ~ LookupValueType x ('ForkTree l (Node n a1) r), o ~ CmpNat x n,
  Lookupable' x a ('ForkTree l (Node n a1) r) o) =>
  Lookupable x a ('ForkTree l (Node n a1) r) where
  lookup x t = lookup' x t (Proxy::Proxy o)

-- | This type class provides the functionality to lookup a node with key 'x'
-- in a non empty tree 't' without checking any structural invariant (key ordering).
-- It's only used by the 'Lookupable' class and it has one extra parameter 'o',
-- which is the type level comparison of 'x' with the key value of the root node.
-- The 'o' parameter guides the lookup.
class Lookupable' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) where
  lookup' :: Proxy x -> ITree t -> Proxy o -> a
instance Lookupable' x a ('ForkTree l (Node n a) r) 'EQ where
  lookup' _ (ForkITree _ node _) _ = getValue node
instance (l ~ 'ForkTree ll (Node ln lna) lr, o ~ CmpNat x ln,
  Lookupable' x a l o) =>
  Lookupable' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT where
  lookup' p (ForkITree l _ _) _ = lookup' p l (Proxy::Proxy o)
instance (r ~ 'ForkTree rl (Node rn rna) rr, o ~ CmpNat x rn,
  Lookupable' x a ('ForkTree rl (Node rn rna) rr) o) =>
  Lookupable' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT where
  lookup' p (ForkITree _ _ r) _ = lookup' p r (Proxy::Proxy o)
