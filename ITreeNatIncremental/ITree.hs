{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ITreeNatIncremental.ITree where

import Data.Type.Bool
import Data.Type.Equality
import Data.Nat
import Prelude hiding (max)

data Tree :: * where
  EmptyTree :: Tree
  ForkTree  :: Tree -> Nat -> Tree -> Tree

data ITree :: Tree -> * where
  EmptyITree :: ITree 'EmptyTree
  ForkITree  :: ITree l -> Natty n -> ITree r -> ITree ('ForkTree l n r)

instance Show (ITree t) where
  show EmptyITree         = "E"
  show (ForkITree l n r)  = "F " ++ go l ++ " " ++ show n ++ " " ++ go r
    where
      go :: ITree t' -> String
      go EmptyITree         = "E"
      go (ForkITree l' n' r')  = "(F " ++ go l' ++ " " ++ show n' ++ " " ++ go r' ++ ")"

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

insert :: Natty x -> ITree t -> ITree (Insert x t)
insert x EmptyITree         = ForkITree EmptyITree x EmptyITree
insert x (ForkITree l n r)  = case owotoNat x n of
  EE -> ForkITree l n r
  LE -> ForkITree (insert x l) n r
  GE -> ForkITree l n (insert x r)

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

member :: Natty x -> ITree t -> Bool
member _ EmptyITree         = False
member x (ForkITree l n r)  = case owotoNat x n of
  EE -> True
  LE -> member x l
  GE -> member x r

type family IsEmpty (t :: Tree) :: Bool where
  IsEmpty 'EmptyTree        = 'True
  IsEmpty ('ForkTree l n r) = 'False

data IET :: Tree -> * where
  E   :: IET 'EmptyTree
  NE  :: IET ('ForkTree l n r)

isEmpty :: ITree t -> IET t
isEmpty EmptyITree  = E
isEmpty ForkITree{} = NE

type family Max (t :: Tree) :: Nat where
  Max ('ForkTree l n r) =
    (If (IsEmpty r == 'True)
      n
      (Max r)
    )

max :: ITree ('ForkTree l n r) -> Natty (Max ('ForkTree l n r))
max (ForkITree _ n r) = case isEmpty r of
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

delete :: Natty x -> ITree t -> ITree (Delete x t)
delete _ EmptyITree         = EmptyITree
delete x (ForkITree l n r)  = case owotoNat x n of
  EE -> case isEmpty l of
    E -> r
    NE -> case isEmpty r of
      E -> l
      NE -> ForkITree (delete maxL l) maxL r
        where
          maxL = max l
  LE -> ForkITree (delete x l) n r
  GE -> ForkITree l n (delete x r)
