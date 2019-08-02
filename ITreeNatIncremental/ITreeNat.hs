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

instance Show (BST t) where
  show (BST t) = "BST $ " ++ show t

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

type family MaxNat (n1 :: Nat) (n2 :: Nat) :: Nat where
  MaxNat n1 n2 =
    (If (Compare n1 n2 == 'EQ)
      n1
      (If (Compare n1 n2 == 'LT)
        n2
        n1
      )
    )

type family Height (t :: Tree) :: Nat where
  Height 'EmptyTree        = 'Z
  Height ('ForkTree l n r) = 'S (MaxNat (Height l) (Height r))

data ProofHeight :: Tree -> Nat -> *  where
  PH :: (Height t ~ h) => ITree t -> Natty h -> ProofHeight t h

proofHeight :: (Height t ~ h) =>
  ITree t -> ProofHeight t h
proofHeight EmptyITree        = PH EmptyITree Zy
proofHeight t@(ForkITree l _ r) = let
  PH _ hl = proofHeight l
  PH _ hr = proofHeight r
  in case owotoNat hl hr of
    EE -> PH t (Sy hr)
    LE -> PH t (Sy hr)
    GE -> PH t (Sy hl)

class BalancedHeights (h1 :: Nat) (h2 :: Nat) where
instance BalancedHeights 'Z      'Z      where
instance BalancedHeights 'Z      ('S 'Z) where
instance BalancedHeights ('S 'Z) 'Z      where
instance BalancedHeights h1 h2 =>
  BalancedHeights ('S h1) ('S h2)

data ProofBalancedHeights :: Nat -> Nat -> * where
  PBH  :: (BalancedHeights h1 h2) =>
    Natty h1 -> Natty h2 -> ProofBalancedHeights h1 h2
  NPBH :: ProofBalancedHeights h1 h2  -- | For completion only

proofBalancedHeights :: Natty h1 -> Natty h2 -> ProofBalancedHeights h1 h2
proofBalancedHeights Zy      Zy      = PBH Zy Zy
proofBalancedHeights Zy      (Sy Zy) = PBH Zy (Sy Zy)
proofBalancedHeights (Sy Zy) Zy      = PBH (Sy Zy) Zy
proofBalancedHeights (Sy h1) (Sy h2) = case proofBalancedHeights h1 h2 of
  PBH h1' h2' -> PBH (Sy h1') (Sy h2')
  NPBH        -> NPBH
proofBalancedHeights Zy          (Sy (Sy _)) = NPBH
proofBalancedHeights (Sy (Sy _)) Zy          = NPBH

class IsBBT (t :: Tree) where
instance IsBBT 'EmptyTree where
instance (IsBBT l, IsBBT r, BalancedHeights (Height l) (Height r)) =>
  IsBBT ('ForkTree l n r) where

data BBT :: Tree -> * where
  BBT :: (IsBBT t) => ITree t -> BBT t

instance Show (BBT t) where
  show (BBT t) = "BBT $ " ++ show t

data ProofBBT :: Tree -> * where
  PBE :: ProofBBT 'EmptyTree
  PBF :: (IsBBT l, IsBBT r, BalancedHeights (Height l) (Height r)) =>
    ProofBBT l -> Natty n -> ProofBBT r -> ProofBBT ('ForkTree l n r)

proofBBT :: ITree t -> ProofBBT t
proofBBT EmptyITree        = PBE
proofBBT (ForkITree l n r) = case proofSubTreeBBT l of
  STBBT _ -> case proofSubTreeBBT r of
    STBBT _ -> case proofHeight l of
      PH _ hl -> case proofHeight r of
        PH _ hr -> case proofBalancedHeights hl hr of
          PBH _ _ -> PBF (proofBBT l) n (proofBBT r)
          NPBH    -> undefined

data SubTreeBBT :: Tree -> * where
  STBBT :: (IsBBT t) =>
    ITree t -> SubTreeBBT t

proofSubTreeBBT :: ITree t -> SubTreeBBT t
proofSubTreeBBT EmptyITree        = STBBT EmptyITree
proofSubTreeBBT (ForkITree l n r) = case proofSubTreeBBT l of
  STBBT _ -> case proofSubTreeBBT r of
    STBBT _ -> case proofHeight l of
      PH _ hl -> case proofHeight r of
        PH _ hr -> case proofBalancedHeights hl hr of
          PBH _ _ -> STBBT (ForkITree l n r)
          NPBH    -> undefined

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

insertBBT :: Natty x -> BBT t -> BBT (Insert x t)
insertBBT x (BBT t) = let
  t' = insert x t
  in case proofBBT t' of
    PBE   -> undefined -- | Impossible case since t' = insert x t
    PBF{} -> BBT t'

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

deleteBBT :: Natty x -> BBT t -> BBT (Delete x t)
deleteBBT x (BBT t) = let
  t' = delete x t
  in case proofBBT t' of
    PBE   -> BBT EmptyITree
    PBF{} -> BBT t'
