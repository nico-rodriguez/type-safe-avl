{-|
Module      : W
Description : 
Copyright   : (c) Nicolás Rodríguez, 2021
License     : GPL-3
Maintainer  : Nicolás Rodríguez
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# LANGUAGE Safe                  #-}

module Data.Tree.BST.Extern.Insert (
  Insertable(Insert, insert), Insertable'(Insert', insert')
) where

import           Data.Kind       (Type)
import           Data.Proxy      (Proxy (Proxy))
import           Data.Tree.ITree (ITree (EmptyITree, ForkITree),
                                  Tree (EmptyTree, ForkTree))
import           Data.Tree.Node  (Node, mkNode, getValue)
import           GHC.TypeNats    (CmpNat, Nat)
import           Prelude         (Ordering (EQ, GT, LT), Show)


-- | This class provides the functionality to insert a node with key 'x' and value type 'a'
-- | in a tree 't' without checking any structural invariant (BST).
-- | The insertion is defined at the value level and the type level, and is performed
-- | as if the tree is a BST; the checking of the BST invariant is performed after the insertion.
class Insertable (x :: Nat) (a :: Type) (t :: Tree) where
  type Insert (x :: Nat) (a :: Type) (t :: Tree) :: Tree
  insert :: Node x a -> ITree t -> ITree (Insert x a t)
instance (Show a) =>
  Insertable x a 'EmptyTree where
  type Insert x a 'EmptyTree = 'ForkTree 'EmptyTree (Node x a) 'EmptyTree
  insert node _ = ForkITree EmptyITree node EmptyITree
instance (o ~ CmpNat x n,
  Insertable' x a ('ForkTree l (Node n a1) r) o) =>
  Insertable x a ('ForkTree l (Node n a1) r) where
  type Insert x a ('ForkTree l (Node n a1) r) = Insert' x a ('ForkTree l (Node n a1) r) (CmpNat x n)
  insert n t = insert' n t (Proxy::Proxy o)

-- | This class provides the functionality to insert a node with key 'x' and value type 'a'
-- | in a non empty tree 't' without checking any structural invariant (BST).
-- | It's only used by the 'Insertable' class and it has one extra parameter 'o',
-- | which is the type level comparison of 'x' with the key value of the root node.
-- | The 'o' parameter guides the insertion.
class Insertable' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) where
  type Insert' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) :: Tree
  insert' :: Node x a -> ITree t -> Proxy o -> ITree (Insert' x a t o)
instance (Show a) =>
  Insertable' x a ('ForkTree l (Node n a1) r) 'EQ where
  type Insert' x a ('ForkTree l (Node n a1) r) 'EQ = 'ForkTree l (Node n a) r
  insert' node (ForkITree l _ r) _ = ForkITree l node' r
    where
      node' = mkNode (Proxy::Proxy n) (getValue node)
instance (Show a) =>
  Insertable' x a ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  type Insert' x a ('ForkTree 'EmptyTree (Node n a1) r) 'LT = 'ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n a1) r
  insert' node' (ForkITree _ node r) _ = ForkITree (ForkITree EmptyITree node' EmptyITree) node r
instance (l ~ 'ForkTree ll (Node ln lna) lr, o ~ CmpNat x ln,
  Insertable' x a l o) =>
  Insertable' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT where
  type Insert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT =
    'ForkTree (Insert' x a ('ForkTree ll (Node ln lna) lr) (CmpNat x ln)) (Node n a1) r
  insert' node (ForkITree l node' r) _ = ForkITree (insert' node l (Proxy::Proxy o)) node' r
instance (Show a) => Insertable' x a ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  type Insert' x a ('ForkTree l (Node n a1) 'EmptyTree) 'GT =
    'ForkTree l (Node n a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree)
  insert' node (ForkITree l node' _) _ = ForkITree l node' (ForkITree EmptyITree node EmptyITree)
instance (r ~ 'ForkTree rl (Node rn rna) rr, o ~ CmpNat x rn,
  Insertable' x a r o) =>
  Insertable' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT where
  type Insert' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT =
    'ForkTree l (Node n a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn))
  insert' node (ForkITree l node' r) _ = ForkITree l node' (insert' node r (Proxy::Proxy o))
