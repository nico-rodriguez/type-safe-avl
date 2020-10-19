{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Tree.AVL.Extern.Delete (
  MaxKeyDeletable(MaxKeyDelete,maxKeyDelete),
  Deletable(Delete,delete), Deletable'(Delete')
) where

import           Data.Proxy                   (Proxy (Proxy))
import           Data.Tree.AVL.Extern.Balance (Balanceable (Balance, balance))
import           Data.Tree.BST.Extern.Delete  (Maxable (MaxKey, MaxValue, maxValue))
import           Data.Tree.ITree              (ITree (EmptyITree, ForkITree),
                                               Tree (EmptyTree, ForkTree))
import           Data.Tree.Node               (Node (Node))
import           GHC.TypeNats                 (CmpNat, Nat)
import           Prelude                      (Ordering (EQ, GT, LT), Show, ($))


-- | This class provides the functionality to delete the node with maximum key value
-- | in a tree 't' without checking any structural invariant (BST).
-- | The deletion is defined at the value level and the type level, and is performed
-- | as if the tree is a BST; the checking of the BST invariant is performed after the deletion.
class MaxKeyDeletable (t :: Tree) where
  type MaxKeyDelete (t :: Tree) :: Tree
  maxKeyDelete :: (t ~ 'ForkTree l (Node n a1) r) =>
    ITree t -> ITree (MaxKeyDelete t)
instance MaxKeyDeletable 'EmptyTree where
  type MaxKeyDelete 'EmptyTree = 'EmptyTree
  maxKeyDelete EmptyITree = EmptyITree
instance MaxKeyDeletable ('ForkTree l (Node n a1) 'EmptyTree) where
  type MaxKeyDelete ('ForkTree l (Node n a1) 'EmptyTree) = l
  maxKeyDelete (ForkITree l (Node _) EmptyITree) = l
instance (r ~ 'ForkTree rl (Node rn ra) rr, MaxKeyDeletable ('ForkTree rl (Node rn ra) rr),
  Balanceable ('ForkTree l (Node n a1) (MaxKeyDelete r))) =>
  MaxKeyDeletable ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) where
  type MaxKeyDelete ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) =
    Balance ('ForkTree l (Node n a1) (MaxKeyDelete ('ForkTree rl (Node rn ra) rr)))
  maxKeyDelete (ForkITree l node r@ForkITree{}) =
    balance $ ForkITree l node (maxKeyDelete r)


-- | This class provides the functionality to delete the node with key 'x'
-- | in a tree 't' without checking any structural invariant (BST/AVL).
-- | The deletion is defined at the value level and the type level, and is performed
-- | as if the tree is a BST/AVL; the checking of the BST/AVL invariant is performed after the deletion.
class Deletable (x :: Nat) (t :: Tree) where
  type Delete (x :: Nat) (t :: Tree) :: Tree
  delete :: Proxy x -> ITree t -> ITree (Delete x t)
instance Deletable x 'EmptyTree where
  type Delete x 'EmptyTree = 'EmptyTree
  delete _ EmptyITree = EmptyITree
instance (Deletable' x ('ForkTree l (Node n a1) r) (CmpNat x n)) =>
  Deletable x ('ForkTree l (Node n a1) r) where
  type Delete x ('ForkTree l (Node n a1) r) = Delete' x ('ForkTree l (Node n a1) r) (CmpNat x n)
  delete px t = delete' px t (Proxy::Proxy (CmpNat x n))

-- | This class provides the functionality to delete a node with key 'x'
-- | in a non empty tree 't' without checking any structural invariant (BST/AVL).
-- | It's only used by the 'Deletable' class and it has one extra parameter 'o',
-- | which is the type level comparison of 'x' with the key value of the root node.
-- | The 'o' parameter guides the insertion.
class Deletable' (x :: Nat) (t :: Tree) (o :: Ordering) where
  type Delete' (x :: Nat) (t :: Tree) (o :: Ordering) :: Tree
  delete' :: Proxy x -> ITree t -> Proxy o -> ITree (Delete' x t o)
instance Deletable' x ('ForkTree 'EmptyTree (Node n a1) 'EmptyTree) 'EQ where
  type Delete' x ('ForkTree 'EmptyTree (Node n a1) 'EmptyTree) 'EQ = 'EmptyTree
  delete' _ (ForkITree EmptyITree (Node _) EmptyITree) _ = EmptyITree
instance Deletable' x ('ForkTree 'EmptyTree (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  type Delete' x ('ForkTree 'EmptyTree (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ = ('ForkTree rl (Node rn ra) rr)
  delete' _ (ForkITree EmptyITree (Node _) r@ForkITree{}) _ = r
instance Deletable' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) 'EmptyTree) 'EQ where
  type Delete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) 'EmptyTree) 'EQ = ('ForkTree ll (Node ln la) lr)
  delete' _ (ForkITree l@ForkITree{} (Node _) EmptyITree) _ = l
instance (Show (MaxValue ('ForkTree ll (Node ln la) lr)), MaxKeyDeletable ('ForkTree ll (Node ln la) lr), Maxable ('ForkTree ll (Node ln la) lr),
  Balanceable ('ForkTree (MaxKeyDelete ('ForkTree ll (Node ln la) lr)) (Node (MaxKey ('ForkTree ll (Node ln la) lr)) (MaxValue ('ForkTree ll (Node ln la) lr))) ('ForkTree rl (Node rn ra) rr))) =>
  Deletable' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  type Delete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ =
    Balance ('ForkTree (MaxKeyDelete ('ForkTree ll (Node ln la) lr)) (Node (MaxKey ('ForkTree ll (Node ln la) lr)) (MaxValue ('ForkTree ll (Node ln la) lr))) ('ForkTree rl (Node rn ra) rr))
  delete' _ (ForkITree l@ForkITree{} (Node _) r@ForkITree{}) _ =
    balance $ ForkITree (maxKeyDelete l) (Node (maxValue l)::Node (MaxKey ('ForkTree ll (Node ln la) lr)) (MaxValue ('ForkTree ll (Node ln la) lr))) r
instance Deletable' x ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  type Delete' x ('ForkTree 'EmptyTree (Node n a1) r) 'LT = ('ForkTree 'EmptyTree (Node n a1) r)
  delete' _ t@(ForkITree EmptyITree (Node _) _) _ = t
instance (Deletable' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln),
  Balanceable ('ForkTree (Delete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln)) (Node n a1) r)) =>
  Deletable' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) r) 'LT where
  type Delete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) r) 'LT =
    Balance ('ForkTree (Delete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln)) (Node n a1) r)
  delete' px (ForkITree l@ForkITree{} node r) _ = balance $ ForkITree (delete' px l (Proxy::Proxy (CmpNat x ln))) node r
instance Deletable' x ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  type Delete' x ('ForkTree l (Node n a1) 'EmptyTree) 'GT = ('ForkTree l (Node n a1) 'EmptyTree)
  delete' _ t@(ForkITree _ (Node _) EmptyITree) _ = t
instance (Deletable' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn),
  Balanceable ('ForkTree l (Node n a1) (Delete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn)))) =>
  Deletable' x ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'GT where
  type Delete' x ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'GT =
    Balance ('ForkTree l (Node n a1) (Delete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn)))
  delete' px (ForkITree l node r@ForkITree{}) _ = balance $ ForkITree l node (delete' px r (Proxy::Proxy (CmpNat x rn)))