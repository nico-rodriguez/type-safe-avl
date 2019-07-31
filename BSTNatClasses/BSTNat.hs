{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module BSTNatClasses.BSTNat where

import Compare (Compare, OWOTO(..))
import Data.Type.Bool
import Data.Type.Equality
import Data.Nat
import Prelude hiding (max)


data Tree :: * where
  EmptyTree :: Tree
  ForkTree  :: Tree -> Nat -> Tree -> Tree

class IsBST (t :: Tree) where
instance IsBST 'EmptyTree where
instance (IsBST l, IsBST r, LtN l n, GtN r n) => IsBST ('ForkTree l n r) where

class LtN (t :: Tree) (x :: Nat) where
instance LtN 'EmptyTree x where
instance (LtN l x, Compare n x ~ 'LT, LtN r x) => LtN ('ForkTree l n r) x

class GtN (t :: Tree) (x :: Nat) where
instance GtN 'EmptyTree x where
instance (GtN l x, Compare n x ~ 'GT, GtN r x) => GtN ('ForkTree l n r) x

data BST :: Tree -> * where
  EmptyBST :: BST 'EmptyTree
  ForkBST  :: (IsBST ('ForkTree l n r), GtN r n, LtN l n) =>
    BST l -> Natty n -> BST r -> BST ('ForkTree l n r)

instance Show (BST t) where
  show EmptyBST         = "E"
  show (ForkBST l n r)  = "F " ++ go l ++ " " ++ show n ++ " " ++ go r
    where
      go :: BST t' -> String
      go EmptyBST         = "E"
      go (ForkBST l' n' r')  = "(" ++ go l' ++ " " ++ show n' ++ " " ++ go r' ++ ")"

type family Insert (x :: Nat) (t :: Tree) :: Tree where
  Insert x 'EmptyTree         = 'ForkTree 'EmptyTree x 'EmptyTree
  Insert x ('ForkTree l n r)  =
    (If (Compare x n == 'EQ)
      ('ForkTree l n r)
      (If (Compare x n == 'LT)
        ('ForkTree (Insert x l) n r)
        ('ForkTree l n (Insert x r))
      )
    )

insert :: Natty n -> BST t -> BST (Insert n t)
insert x EmptyBST         = ForkBST EmptyBST x EmptyBST
insert x (ForkBST l n r)  = case owotoNat x n of
  EE -> ForkBST l n r
  LE -> ForkBST (insert x l) n r
  GE -> ForkBST l n (insert x r)

type family Member (x :: Nat) (t :: Tree) :: Bool where
  Member x 'EmptyTree         = 'False
  Member x ('ForkTree l n r)  =
    (If (Compare x n == 'EQ)
      'True
      (If (Compare x n == 'LT)
        (Member x l)
        (Member x r)
      )
    )

member :: Natty n -> BST t -> Bool
member _ EmptyBST         = False
member x (ForkBST l n r)  = case owotoNat x n of
  EE -> True
  LE -> member x l
  GE -> member x r

type family IsEmpty (t :: Tree) :: Bool where
  IsEmpty 'EmptyTree        = 'True
  IsEmpty ('ForkTree l n r) = 'False

data IET :: Tree -> * where
  E   :: IET 'EmptyTree
  NE  :: IET ('ForkTree l n r)

isEmpty :: BST t -> IET t
isEmpty EmptyBST  = E
isEmpty ForkBST{} = NE

type family Max (t :: Tree) :: Nat where
  Max ('ForkTree l n r) =
    (If (IsEmpty r == 'True)
      n
      (Max r)
    )

max :: BST ('ForkTree l n r) -> Natty (Max ('ForkTree l n r))
max (ForkBST _ n r) = case isEmpty r of
  E -> n
  NE -> max r

type family Delete (x :: Nat) (t :: Tree) :: Tree where
  Delete x 'EmptyTree         = 'EmptyTree
  Delete x ('ForkTree l n r)  =
    (If (Compare x n == 'EQ)
      (If (IsEmpty l == 'True)
        r
        (If (IsEmpty r == 'True)
          l
          ('ForkTree (Delete (Max l) l) (Max l) r)
        )
      )
      (If (Compare x n == 'LT)
        ('ForkTree (Delete x l) n r)
        ('ForkTree l n (Delete x r))
      )
    )

delete :: Natty x -> BST t -> BST (Delete x t)
delete _ EmptyBST         = EmptyBST
delete x (ForkBST l n r)  = case owotoNat x n of
  EE -> case isEmpty l of
    E -> r
    NE -> case isEmpty r of
      E -> l
      NE -> ForkBST (delete maxL l) maxL r
        where
          maxL = max l
  LE -> ForkBST (delete x l) n r
  GE -> ForkBST l n (delete x r)
