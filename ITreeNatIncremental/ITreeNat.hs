{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ITreeNatIncremental.ITreeNat where

import Compare (Compare, OWOTO(..))
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
      go (ForkITree l' n' r')  = "(" ++ go l' ++ " " ++ show n' ++ " " ++ go r' ++ ")"

class IsBST (t :: Tree) where
instance IsBST 'EmptyTree where
instance (IsBST l, IsBST r, LtN l n, GtN r n) =>
  IsBST ('ForkTree l n r) where

class LtN (t :: Tree) (x :: Nat) where
instance LtN 'EmptyTree x where
instance (LtN l x, Compare n x ~ 'LT, LtN r x) =>
  LtN ('ForkTree l n r) x

class GtN (t :: Tree) (x :: Nat) where
instance GtN 'EmptyTree x where
instance (GtN l x, Compare n x ~ 'GT, GtN r x) =>
  GtN ('ForkTree l n r) x

data BST :: Tree -> * where
  BST :: (IsBST t) => ITree t -> BST t

data ProofBST :: Tree -> * where
  PE :: ProofBST 'EmptyTree
  PF :: (IsBST l, IsBST r, LtN l n, GtN r n, IsBST ('ForkTree l n r)) =>
    ProofBST l -> Natty n -> ProofBST r -> ProofBST ('ForkTree l n r)

proofBST :: ITree t -> ProofBST t
proofBST EmptyITree        = PE
proofBST (ForkITree l n r) = case proofGTN r n of
  PGTN _ _ -> case proofLTN l n of
    PLTN _ _ -> PF (proofBST l) n (proofBST r)

data LTN :: Tree -> Nat -> * where
  PLTN :: (LtN t n, IsBST t) =>
    ITree t -> Natty n -> LTN t n

proofLTN :: ITree t -> Natty n -> LTN t n
proofLTN EmptyITree         n = PLTN EmptyITree n
proofLTN t@(ForkITree l n1 r) n = case proofLTN l n of
  PLTN _ _ -> case proofLTN r n of
    PLTN _ _ -> case owotoNat n1 n of
      EE -> undefined -- | Impossible case since we want to prove LtN t n
      LE -> case proofBST t of
        PF{} -> PLTN t n
      GE -> undefined -- | Impossible case since we want to prove LtN t n

data GTN :: Tree -> Nat -> * where
  PGTN :: (GtN t n, IsBST t) =>
    ITree t -> Natty n -> GTN t n

proofGTN :: ITree t -> Natty n -> GTN t n
proofGTN EmptyITree           n = PGTN EmptyITree n
proofGTN t@(ForkITree l n1 r) n = case proofGTN r n of
  PGTN _ _ -> case proofGTN l n of
    PGTN _ _ -> case owotoNat n1 n of
      EE -> undefined -- | Impossible case since we want to prove GtN t n
      LE -> undefined -- | Impossible case since we want to prove GtN t n
      GE -> case proofBST t of
        PF{} -> PGTN t n

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

insertBST :: Natty x -> BST t -> BST (Insert x t)
insertBST x (BST t) = let
  t' = insert x t
  in case proofBST t' of
    PE   -> undefined -- | Impossible case since t' = insert x t
    PF{} -> BST t'

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

deleteBST :: Natty x -> BST t -> BST (Delete x t)
deleteBST x (BST t) = let
  t' = delete x t
  in case proofBST t' of
    PE   -> BST EmptyITree
    PF{} -> BST t'
