{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# LANGUAGE Safe                  #-}

module Data.Tree.BST.Extern.Delete (
  Maxable(MaxKey, MaxValue, maxValue),
  MaxKeyDeletable(MaxKeyDelete,maxKeyDelete),
  Deletable(Delete, delete), Deletable'(Delete', delete')
) where

import           Data.Kind       (Type)
import           Data.Proxy      (Proxy (Proxy))
import           Data.Tree.ITree (ITree (EmptyITree, ForkITree),
                                  Tree (EmptyTree, ForkTree))
import           Data.Tree.Node  (Node, getValue, mkNode)
import           GHC.TypeNats    (CmpNat, Nat)
import           Prelude         (Ordering (EQ, GT, LT), Show)


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
  maxKeyDelete _ = EmptyITree
instance MaxKeyDeletable ('ForkTree l (Node n a1) 'EmptyTree) where
  type MaxKeyDelete ('ForkTree l (Node n a1) 'EmptyTree) = l
  maxKeyDelete (ForkITree l _ _) = l
instance (MaxKeyDeletable ('ForkTree rl (Node rn ra) rr)) =>
  MaxKeyDeletable ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) where
  type MaxKeyDelete ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) =
    'ForkTree l (Node n a1) (MaxKeyDelete ('ForkTree rl (Node rn ra) rr))
  maxKeyDelete (ForkITree l node r) =
    ForkITree l node (maxKeyDelete r)


-- | This class provides the functionality to get the key, type and value of the node with maximum key value
-- | in a tree 't' without checking any structural invariant (BST).
-- | The lookup is defined at the value level and the type level, and is performed
-- | as if the tree is a BST.
-- | Since the keys are only kept at the type level, there's no value level getter of the maximum key.
class Maxable (t :: Tree) where
  type MaxKey (t :: Tree) :: Nat
  type MaxValue (t :: Tree) :: Type
  maxValue :: (t ~ 'ForkTree l (Node n a1) r) =>
    ITree t -> MaxValue t
instance Maxable ('ForkTree l (Node n a1) 'EmptyTree) where
  type MaxKey ('ForkTree l (Node n a1) 'EmptyTree) = n
  type MaxValue ('ForkTree l (Node n a1) 'EmptyTree) = a1
  maxValue (ForkITree _ node EmptyITree) = getValue node
instance (Maxable ('ForkTree rl (Node rn ra) rr)) =>
  Maxable ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) where
  type MaxKey ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) = MaxKey ('ForkTree rl (Node rn ra) rr)
  type MaxValue ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) = MaxValue ('ForkTree rl (Node rn ra) rr)
  maxValue (ForkITree _ _ r) = maxValue r



-- | This class provides the functionality to delete the node with key 'x'
-- | in a tree 't' without checking any structural invariant (BST).
-- | The deletion is defined at the value level and the type level, and is performed
-- | as if the tree is a BST; the checking of the BST invariant is performed after the deletion.
class Deletable (x :: Nat) (t :: Tree) where
  type Delete (x :: Nat) (t :: Tree) :: Tree
  delete :: Proxy x -> ITree t -> ITree (Delete x t)
instance Deletable x 'EmptyTree where
  type Delete x 'EmptyTree = 'EmptyTree
  delete _ _ = EmptyITree
instance (o ~ CmpNat x n,
  Deletable' x ('ForkTree l (Node n a1) r) o) =>
  Deletable x ('ForkTree l (Node n a1) r) where
  type Delete x ('ForkTree l (Node n a1) r) = Delete' x ('ForkTree l (Node n a1) r) (CmpNat x n)
  delete px t = delete' px t (Proxy::Proxy o)

-- | This class provides the functionality to delete a node with key 'x'
-- | in a non empty tree 't' without checking any structural invariant (BST).
-- | It's only used by the 'Deletable' class and it has one extra parameter 'o',
-- | which is the type level comparison of 'x' with the key value of the root node.
-- | The 'o' parameter guides the insertion.
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
instance (l ~ 'ForkTree ll (Node ln la) lr,
  Show (MaxValue l), MaxKeyDeletable l, Maxable l) =>
  Deletable' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ where
  type Delete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'EQ =
    'ForkTree (MaxKeyDelete ('ForkTree ll (Node ln la) lr))
              (Node (MaxKey ('ForkTree ll (Node ln la) lr)) (MaxValue ('ForkTree ll (Node ln la) lr)))
              ('ForkTree rl (Node rn ra) rr)
  delete' _ (ForkITree l _ r) _ =
    ForkITree l' node' r
      where
        l'    = maxKeyDelete l
        node' = mkNode (Proxy::Proxy (MaxKey l)) (maxValue l)
instance Deletable' x ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  type Delete' x ('ForkTree 'EmptyTree (Node n a1) r) 'LT = ('ForkTree 'EmptyTree (Node n a1) r)
  delete' _ t _ = t
instance (o ~ CmpNat x ln,
  Deletable' x ('ForkTree ll (Node ln la) lr) o) =>
  Deletable' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) r) 'LT where
  type Delete' x ('ForkTree ('ForkTree ll (Node ln la) lr) (Node n a1) r) 'LT =
    'ForkTree (Delete' x ('ForkTree ll (Node ln la) lr) (CmpNat x ln)) (Node n a1) r
  delete' px (ForkITree l node r) _ = ForkITree (delete' px l (Proxy::Proxy o)) node r
instance Deletable' x ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  type Delete' x ('ForkTree l (Node n a1) 'EmptyTree) 'GT = ('ForkTree l (Node n a1) 'EmptyTree)
  delete' _ t _ = t
instance (o ~ CmpNat x rn,
  Deletable' x ('ForkTree rl (Node rn ra) rr) o) =>
  Deletable' x ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'GT where
  type Delete' x ('ForkTree l (Node n a1) ('ForkTree rl (Node rn ra) rr)) 'GT =
    'ForkTree l (Node n a1) (Delete' x ('ForkTree rl (Node rn ra) rr) (CmpNat x rn))
  delete' px (ForkITree l node r) _ = ForkITree l node (delete' px r (Proxy::Proxy o))
