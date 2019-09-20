{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Binary trees without constraints. The functions insert, member, max and delete
-- | assume the trees are binary search, but this constraint is not included in
-- | the types of the trees.

module ITreeNatIncremental.ITree where

import           Data.Kind
import           Data.Proxy
import           Data.Type.Bool
import           Data.Type.Equality
import           GHC.TypeLits
import           ITreeNatIncremental.Node

data Tree :: Type where
  EmptyTree :: Tree
  ForkTree  :: Tree -> n -> Tree -> Tree

-- | Check if all elements of the tree are strictly less than x
type family LtN (l :: Tree) (x :: Nat) :: Bool where
  LtN 'EmptyTree        x = 'True
  LtN ('ForkTree l (Node n a) r) x = CmpNat n x == 'LT && LtN l x && LtN r x


-- | Check if all elements of the tree are strictly greater than x
type family GtN (r :: Tree) (x :: Nat) :: Bool where
  GtN 'EmptyTree        x = 'True
  GtN ('ForkTree l (Node n a) r) x = CmpNat n x == 'GT && GtN l x && GtN r x


data ITree :: Tree -> Type where
  EmptyITree :: ITree 'EmptyTree
  ForkITree  :: Show a => ITree l -> Node n a -> ITree r -> ITree ('ForkTree l (Node n a) r)

instance Show (ITree t) where
  show EmptyITree         = "E"
  show (ForkITree l n@(Node _) r)  = "F " ++ go l ++ " " ++ show n ++ " " ++ go r
    where
      go :: ITree t' -> String
      go EmptyITree         = "E"
      go (ForkITree l' n'@(Node _) r')  = "(F " ++ go l' ++ " " ++ show n' ++ " " ++ go r' ++ ")"


class Insertable (x :: Nat) (a :: Type) (t :: Tree) where
  type Insert (x :: Nat) (a :: Type) (t :: Tree) :: Tree
  insert :: Node x a -> ITree t -> ITree (Insert x a t)
instance Show a => Insertable x a 'EmptyTree where
  type Insert x a 'EmptyTree = 'ForkTree 'EmptyTree (Node x a) 'EmptyTree
  insert (Node a) EmptyITree         = ForkITree EmptyITree (Node a::Node x a) EmptyITree
instance Insertable' x a ('ForkTree l (Node n a1) r) (CmpNat x n) => Insertable x a ('ForkTree l (Node n a1) r) where
  type Insert x a ('ForkTree l (Node n a1) r) = Insert' x a ('ForkTree l (Node n a1) r) (CmpNat x n)
  insert n t = insert' n t (Proxy::Proxy (CmpNat x n))

class Insertable' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) where
  type Insert' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) :: Tree
  insert' :: Node x a -> ITree t -> Proxy o -> ITree (Insert x a t)
instance (Show a, CmpNat x n ~ 'EQ) => Insertable' x a ('ForkTree l (Node n a1) r) 'EQ where
  type Insert' x a ('ForkTree l (Node n a1) r) 'EQ = 'ForkTree l (Node n a) r
  insert' (Node a) (ForkITree l (Node _) r) _ = ForkITree l (Node a::Node n a) r
instance (Show a, CmpNat x n ~ 'LT) => Insertable' x a ('ForkTree 'EmptyTree (Node n a1) r) 'LT where
  type Insert' x a ('ForkTree 'EmptyTree (Node n a1) r) 'LT = 'ForkTree ('ForkTree 'EmptyTree (Node x a) 'EmptyTree) (Node n a1) r
  insert' (Node a) (ForkITree EmptyITree n r) _ = ForkITree (ForkITree EmptyITree (Node a::Node x a) EmptyITree) n r
instance (CmpNat x n ~ 'LT, l ~ 'ForkTree ll (Node ln lna) lr, Insertable' x a l (CmpNat x ln)) => Insertable' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT where
  type Insert' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT = 'ForkTree (Insert' x a ('ForkTree ll (Node ln lna) lr) (CmpNat x ln)) (Node n a1) r
  insert' (Node a) (ForkITree l@ForkITree{} n r) _ = ForkITree (insert' (Node a::Node x a) l (Proxy::Proxy (CmpNat x ln))) n r
instance (Show a, CmpNat x n ~ 'GT) => Insertable' x a ('ForkTree l (Node n a1) 'EmptyTree) 'GT where
  type Insert' x a ('ForkTree l (Node n a1) 'EmptyTree) 'GT = 'ForkTree l (Node n a1) ('ForkTree 'EmptyTree (Node x a) 'EmptyTree)
  insert' (Node a) (ForkITree l n EmptyITree) _ = ForkITree l n (ForkITree EmptyITree (Node a::Node x a) EmptyITree)
instance (CmpNat x n ~ 'GT, r ~ 'ForkTree rl (Node rn rna) rr, Insertable' x a r (CmpNat x rn)) => Insertable' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT where
  type Insert' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT = 'ForkTree l (Node n a1) (Insert' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn))
  insert' (Node a) (ForkITree l n r@ForkITree{}) _ = ForkITree l n (insert' (Node a::Node x a) r (Proxy::Proxy (CmpNat x rn)))


type family Member (x :: Nat) (t :: Tree) :: Bool where
  Member x 'EmptyTree = 'False
  Member x ('ForkTree l (Node n a) r) =
    (If (CmpNat x n == 'EQ)
      'True
      (If (CmpNat x n == 'LT)
        (Member x l)
        (Member x r)
      )
    )

type family LookupValueType (x :: Nat) (t :: Tree) :: Type where
  LookupValueType x ('ForkTree l (Node n a) r) =
    (If (CmpNat x n == 'EQ)
      a
      (If (CmpNat x n == 'LT)
        (LookupValueType x l)
        (LookupValueType x r)
      )
    )

class Lookupable (x :: Nat) (a :: Type) (t :: Tree) where
  lookup :: (t ~ 'ForkTree l (Node n a1) r, Member x t ~ 'True) =>
    Proxy x -> ITree t -> a
instance (Lookupable' x a ('ForkTree l (Node n a1) r) (CmpNat x n), a ~ LookupValueType x ('ForkTree l (Node n a1) r)) =>
  Lookupable x a ('ForkTree l (Node n a1) r) where
  lookup x t = lookup' x t (Proxy::Proxy (CmpNat x n))

class Lookupable' (x :: Nat) (a :: Type) (t :: Tree) (o :: Ordering) where
  lookup' :: (t ~ 'ForkTree l (Node n a1) r, Member x t ~ 'True) =>
    Proxy x -> ITree t -> Proxy o -> a
instance (CmpNat x n ~ 'EQ) => Lookupable' x a ('ForkTree l (Node n a) r) 'EQ where
  lookup' _ (ForkITree _ (Node a) _) _ = getValue (Node a::Node n a)
instance (CmpNat x n ~ 'LT, l ~ 'ForkTree ll (Node ln lna) lr, Member x l ~ 'True, Lookupable' x a l (CmpNat x ln)) =>
  Lookupable' x a ('ForkTree ('ForkTree ll (Node ln lna) lr) (Node n a1) r) 'LT where
  lookup' p (ForkITree l@ForkITree{} _ _) _ = lookup' p l (Proxy::Proxy (CmpNat x ln))
instance (CmpNat x n ~ 'GT, r ~ 'ForkTree rl (Node rn rna) rr, Member x r ~ 'True, Lookupable' x a ('ForkTree rl (Node rn rna) rr) (CmpNat x rn)) =>
  Lookupable' x a ('ForkTree l (Node n a1) ('ForkTree rl (Node rn rna) rr)) 'GT where
  lookup' p (ForkITree _ _ r@ForkITree{}) _ = lookup' p r (Proxy::Proxy (CmpNat x rn))
