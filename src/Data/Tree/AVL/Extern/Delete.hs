{-|
Module      : Data.Tree.AVL.Extern.Delete
Description : Deletion algorithm over ITree trees
Copyright   : (c) Nicolás Rodríguez, 2021
License     : GPL-3
Maintainer  : Nicolás Rodríguez
Stability   : experimental
Portability : POSIX

Implementation of the deletion algorithm over ITree trees for
externalist AVL trees.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# LANGUAGE Safe                  #-}

module Data.Tree.AVL.Extern.Delete (
  MaxKeyDeletable(MaxKeyDelete,maxKeyDelete),
  Deletable(Delete,delete), Deletable'(Delete')
) where

import           Data.Proxy                   (Proxy (Proxy))
import           Data.Tree.AVL.Extern.Balance (Balanceable (Balance, balance))
import           Data.Tree.BST.Extern.Delete  (Maxable (MaxKey, MaxValue, maxValue))
import           Data.Tree.ITree              (ITree (EmptyITree, ForkITree),
                                               Tree (EmptyTree, ForkTree))
import           Data.Tree.Node               (Node, mkNode)
import           GHC.TypeNats                 (CmpNat, Nat)
import           Prelude                      (Ordering (EQ, GT, LT), Show, ($))


-- | This type class provides the functionality to delete the node with maximum key value
-- in a tree @t@ without checking any structural invariant (key ordering or height balance).
-- The deletion is defined at the value level and the type level, and is performed
-- as if the tree is an `Data.Tree.AVL.Extern.Constructors.AVL`; the verification of the @AVL@ restrictions is performed after the deletion.
class MaxKeyDeletable (t :: Tree) where
  type MaxKeyDelete (t :: Tree) :: Tree
  maxKeyDelete :: (t ~ 'ForkTree l (Node n a1) r) =>
    ITree t -> ITree (MaxKeyDelete t)
instance MaxKeyDeletable 'EmptyTree where
  type MaxKeyDelete 'EmptyTree = 'EmptyTree
  maxKeyDelete _ = EmptyITree
instance MaxKeyDeletable ('ForkTree l (Node n a1) 'EmptyTree) where
  type MaxKeyDelete ('ForkTree l (Node n a1) 'EmptyTree) = l
  maxKeyDelete (ForkITree l _ _) = l
instance (r ~ 'ForkTree rl (Node rn ra) rr,
  MaxKeyDeletable r, Balanceable ('ForkTree l (Node n a1) (MaxKeyDelete r))) =>
  MaxKeyDeletable ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) where
  type MaxKeyDelete ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) =
    Balance ('ForkTree l (Node n a1) (MaxKeyDelete ('ForkTree rl (Node rn ra) rr)))
  maxKeyDelete (ForkITree l node r) =
    balance $ ForkITree l node (maxKeyDelete r)


-- | This type class provides the functionality to delete the node with key @x@
-- in a tree @t@ without checking any structural invariant (key ordering or height balance).
-- The deletion is defined at the value level and the type level, and is performed
-- as if the tree is a `Data.Tree.AVL.Extern.Constructors.AVL`; the verification of the @AVL@ restrictions is performed after the deletion.
class Deletable (x :: Nat) (t :: Tree) where
  type Delete (x :: Nat) (t :: Tree) :: Tree
  -- | Delete the node with the given key.
  -- If the key is not in the tree, return the same tree.
  delete :: Proxy x -> ITree t -> ITree (Delete x t)
instance Deletable x 'EmptyTree where
  type Delete x 'EmptyTree = 'EmptyTree
  delete _ _ = EmptyITree
instance (o ~ CmpNat x n,
  Deletable' x ('ForkTree l (Node n a1) r) o) =>
  Deletable x ('ForkTree l (Node n a1) r) where
  type Delete x ('ForkTree l (Node n a1) r) = Delete' x ('ForkTree l (Node n a1) r) (CmpNat x n)
  delete px t = delete' px t (Proxy::Proxy o)

-- | This type class provides the functionality to delete a node with key @x@
-- in a non empty tree @t@ without checking any structural invariant (key ordering or height balance).
-- It's only used by the 'Deletable' type class and it has one extra parameter @o@,
-- which is the type level comparison of @x@ with the key value of the root node.
-- The @o@ parameter guides the deletion.
class Deletable' (x :: Nat) (t :: Tree) (o :: Ordering) where
  type Delete' (x :: Nat) (t :: Tree) (o :: Ordering) :: Tree
  delete' :: Proxy x -> ITree t -> Proxy o -> ITree (Delete' x t o)
instance Deletable' x ('ForkTree 'EmptyTree (Node n a1) 'EmptyTree) 'EQ where
  type Delete' x ('ForkTree 'EmptyTree (Node n a1) 'EmptyTree) 'EQ = 'EmptyTree
  delete' _ _ _ = EmptyITree
instance Deletable' x ('ForkTree 'EmptyTree (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  type Delete' x ('ForkTree 'EmptyTree (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ = 'ForkTree rl (Node rn ra) rr
  delete' _ (ForkITree _ _ r) _ = r
instance Deletable' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) 'EmptyTree) 'EQ where
  type Delete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) 'EmptyTree) 'EQ = 'ForkTree ll (Node ln la) lr
  delete' _ (ForkITree l _ _) _ = l
instance (l ~ 'ForkTree ll (Node ln la) lr, r ~ 'ForkTree rl (Node rn ra) rr,
  Show (MaxValue l), MaxKeyDeletable l, Maxable l, Balanceable ('ForkTree (MaxKeyDelete l) (Node (MaxKey l) (MaxValue l)) r)) =>
  Deletable' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  type Delete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ =
    Balance ('ForkTree (MaxKeyDelete ('ForkTree ll (Node ln la) lr)) (Node (MaxKey ('ForkTree ll (Node ln la) lr)) (MaxValue ('ForkTree ll (Node ln la) lr))) ('ForkTree rl (Node rn ra) rr))
  delete' _ (ForkITree l _ r) _ =
    balance $ ForkITree (maxKeyDelete l) node r
      where
        node = mkNode (Proxy::Proxy (MaxKey l)) (maxValue l)
instance Deletable' x ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  type Delete' x ('ForkTree 'EmptyTree (Node n a1) r) 'LT = 'ForkTree 'EmptyTree (Node n a1) r
  delete' _ t _ = t
instance (l ~ 'ForkTree ll (Node ln la) lr, o ~ CmpNat x ln, Deletable' x l o,
  Balanceable ('ForkTree (Delete' x l (CmpNat x ln)) (Node n a1) r)) =>
  Deletable' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) r) 'LT where
  type Delete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) r) 'LT =
    Balance ('ForkTree (Delete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln)) (Node n a1) r)
  delete' px (ForkITree l node r) _ = balance $ ForkITree (delete' px l (Proxy::Proxy o)) node r
instance Deletable' x ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  type Delete' x ('ForkTree l (Node n a1) 'EmptyTree) 'GT = ('ForkTree l (Node n a1) 'EmptyTree)
  delete' _ t _ = t
instance (o ~ CmpNat x rn, r ~ 'ForkTree rl (Node rn ra) rr,
  Deletable' x r o, Balanceable ('ForkTree l (Node n a1) (Delete' x r o))) =>
  Deletable' x ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'GT where
  type Delete' x ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'GT =
    Balance ('ForkTree l (Node n a1) (Delete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn)))
  delete' px (ForkITree l node r) _ = balance $ ForkITree l node (delete' px r (Proxy::Proxy o))
