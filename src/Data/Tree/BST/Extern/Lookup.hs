{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

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


-- | This class provides the functionality to lookup a node with key 'x'
-- | in a non empty tree 't' without checking any structural invariant (BST).
-- | The lookup is defined at the value level and the type level, and is performed
-- | as if the tree is a BST.
-- | It's necessary to know the type 'a' of the value stored in node with key 'x'
-- | so that the type of the value returned by 'lookup' may be specified.
class Lookupable (x :: Nat) (a :: Type) (t :: Tree) where
  lookup :: (t ~ 'ForkTree l (Node n a1) r, Member x t ~ 'True) =>
    Proxy x -> ITree t -> a
instance (Lookupable' x a ('ForkTree l (Node n a1) r) (CmpNat x n), a ~ LookupValueType x ('ForkTree l (Node n a1) r)) =>
  Lookupable x a ('ForkTree l (Node n a1) r) where
  lookup x t = lookup' x t (Proxy::Proxy (CmpNat x n))

-- | This class provides the functionality to lookup a node with key 'x'
-- | in a non empty tree 't' without checking any structural invariant (BST).
-- | It's only used by the 'Lookupable' class and it has one extra parameter 'o',
-- | which is the type level comparison of 'x' with the key value of the root node.
-- | The 'o' parameter guides the lookup.
class Lookupable' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) where
  lookup' :: Proxy x -> ITree t -> Proxy o -> a
instance Lookupable' x a ('ForkTree l (Node n a) r) 'EQ where
  lookup' _ (ForkITree _ node _) _ = getValue node
instance (l ~ 'ForkTree ll (Node ln lna) lr, Lookupable' x a l (CmpNat x ln)) =>
  Lookupable' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT where
  lookup' p (ForkITree l@ForkITree{} _ _) _ = lookup' p l (Proxy::Proxy (CmpNat x ln))
instance (r ~ 'ForkTree rl (Node rn rna) rr, Lookupable' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn)) =>
  Lookupable' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT where
  lookup' p (ForkITree _ _ r@ForkITree{}) _ = lookup' p r (Proxy::Proxy (CmpNat x rn))
