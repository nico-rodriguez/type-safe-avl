{-# OPTIONS_HADDOCK ignore-exports #-}

{-|
Module      : Data.Tree.AVL.Intern.Lookup
Description : Lookup algorithm over internalist AVL trees
Copyright   : (c) Nicolás Rodríguez, 2021
License     : GPL-3
Maintainer  : Nicolás Rodríguez
Stability   : experimental
Portability : POSIX

Implementation of the lookup algorithm over internalist AVL trees.
Since the lookup does not modify the tree, there is no need for proofs.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# LANGUAGE Safe                  #-}

module Data.Tree.AVL.Intern.Lookup (
  Lookupable(lookup)
) where

import           Data.Kind                        (Type)
import           Data.Proxy                       (Proxy (Proxy))
import           Data.Tree.AVL.Intern.Constructors (AVL (ForkAVL))
import           Data.Tree.BST.Utils              (LookupValueType, Member)
import           Data.Tree.ITree                  (Tree (ForkTree))
import           Data.Tree.Node                   (Node, getValue)
import           GHC.TypeLits                     (CmpNat, Nat)
import           Prelude                          (Bool (True),
                                                   Ordering (EQ, GT, LT))


-- | This type class provides the functionality to lookup a node with key 'x'
-- in a non empty `AVL` 't'.
-- The lookup is defined at the value level and the type level.
-- It's necessary to know the type 'a' of the value stored in node with key 'x'
-- so that the type of the value returned by 'lookup' may be specified.
class Lookupable (x :: Nat) (a :: Type) (t :: Tree) where
  lookup :: (t ~ 'ForkTree l (Node n a1) r, Member x t ~ 'True) =>
    Proxy x -> AVL t -> a
instance (a ~ LookupValueType x ('ForkTree l (Node n a1) r), Lookupable' x a ('ForkTree l (Node n a1) r) (CmpNat x n)) =>
  Lookupable x a ('ForkTree l (Node n a1) r) where
  lookup x t = lookup' x t (Proxy::Proxy (CmpNat x n))

-- | This type class provides the functionality to lookup a node with key 'x'
-- in a non empty `AVL` 't'.
-- It's only used by the 'Lookupable' class and it has one extra parameter 'o',
-- which is the type level comparison of 'x' with the key value of the root node.
-- The 'o' parameter guides the lookup.
class Lookupable' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) where
  lookup' :: Proxy x -> AVL t -> Proxy o -> a
instance Lookupable' x a ('ForkTree l (Node n a) r) 'EQ where
  lookup' _ (ForkAVL _ node _) _ = getValue node
instance (l ~ 'ForkTree ll (Node ln lna) lr, o ~ CmpNat x ln,
  Lookupable' x a l o) =>
  Lookupable' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT where
  lookup' p (ForkAVL l _ _) _ = lookup' p l (Proxy::Proxy o)
instance (r ~ 'ForkTree rl (Node rn rna) rr, o ~ CmpNat x rn,
  Lookupable' x a r o) =>
  Lookupable' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT where
  lookup' p (ForkAVL _ _ r) _ = lookup' p r (Proxy::Proxy o)
