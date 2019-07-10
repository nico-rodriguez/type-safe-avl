{-# LANGUAGE DataKinds #-}

{-# LANGUAGE GADTs #-}

{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE UndecidableInstances #-}

module BST where

import Data.Type.Bool
import Data.Type.Equality
import Prelude hiding (max)

import Nat

data Tree :: * where
  EmptyTree :: Tree
  ForkTree  :: Tree -> Nat -> Tree -> Tree

data BST :: Tree -> * where
  EmptyBST :: BST 'EmptyTree
  ForkBST  :: BST l -> Natty n -> BST r -> BST ('ForkTree l n r)
  -- deriving instance Show (BST t)

instance Show (BST t) where
  show EmptyBST         = "E"
  show (ForkBST l n r)  = "F " ++ go l ++ " " ++ show n ++ " " ++ go r
    where
      go :: BST t' -> String
      go EmptyBST         = "E"
      go (ForkBST l' n' r')  = "(" ++ go l' ++ " " ++ show n' ++ " " ++ go r' ++ ")"

type family Insert (n :: Nat) (t :: Tree) :: Tree where
  Insert n 'EmptyTree         = 'ForkTree 'EmptyTree n 'EmptyTree
  Insert n ('ForkTree l m r)  =
    (If (Compare n m == 'EQ)
      ('ForkTree l m r)
      (If (Compare n m == 'LT)
        ('ForkTree (Insert n l) m r)
        ('ForkTree l m (Insert n r))
      )
    )

insert :: Natty n -> BST t -> BST (Insert n t)
insert n EmptyBST         = ForkBST EmptyBST n EmptyBST
insert n (ForkBST l m r)  = case owoto n m of
  EE -> ForkBST l m r
  LE -> ForkBST (insert n l) m r
  GE -> ForkBST l m (insert n r)

type family Member (n :: Nat) (t :: Tree) :: Bool where
  Member n 'EmptyTree         = 'False
  Member n ('ForkTree l m r)  =
    (If (Compare n m == 'EQ)
      'True
      (If (Compare n m == 'LT)
        (Member n l)
        (Member n r)
      )
    )

member :: Natty n -> BST t -> Bool
member _ EmptyBST         = False
member n (ForkBST l m r)  = case owoto n m of
  EE -> True
  LE -> member n l
  GE -> member n r

type family IsEmpty (t :: Tree) :: Bool where
  IsEmpty 'EmptyTree        = 'True
  IsEmpty ('ForkTree l m r) = 'False

data IET :: Tree -> * where
  E   :: IET 'EmptyTree
  NE  :: IET ('ForkTree l m r)

isEmpty :: BST t -> IET t
isEmpty EmptyBST  = E
isEmpty ForkBST{} = NE

type family Max (t :: Tree) :: Nat where
  Max ('ForkTree l m r) =
    (If (IsEmpty r == 'True)
      m
      (Max r)
    )

max :: BST ('ForkTree l m r) -> Natty (Max ('ForkTree l m r))
max (ForkBST _ n r) = case isEmpty r of
  E -> n
  NE -> max r

type family Delete (n :: Nat) (t :: Tree) :: Tree where
  Delete n 'EmptyTree         = 'EmptyTree
  Delete n ('ForkTree l m r)  =
    (If (Compare n m == 'EQ)
      (If (IsEmpty l == 'True)
        r
        (If (IsEmpty r == 'True)
          l
          ('ForkTree (Delete (Max l) l) (Max l) r)
        )
      )
      (If (Compare n m == 'LT)
        ('ForkTree (Delete n l) m r)
        ('ForkTree l m (Delete n r))
      )
    )

delete :: Natty n -> BST t -> BST (Delete n t)
delete _ EmptyBST         = EmptyBST
delete n (ForkBST l m r)  = case owoto n m of
  EE -> case isEmpty l of
    E -> r
    NE -> case isEmpty r of
      E -> l
      NE -> ForkBST (delete maxL l) maxL r
        where
          maxL = max l
  LE -> ForkBST (delete n l) m r
  GE -> ForkBST l m (delete n r)
