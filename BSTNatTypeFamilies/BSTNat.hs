{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module BSTNatTypeFamilies.BSTNat where

import Data.Type.Bool
import Data.Type.Equality
import Data.Nat (Nat, Compare, Natty, OWOTO(..), owotoNat)
import Prelude hiding (max)


data Tree :: * where
  EmptyTree :: Tree
  ForkTree  :: Tree -> Nat -> Tree -> Tree

type family LtN (l :: Tree) (x :: Nat) :: Bool where
  LtN 'EmptyTree        x = 'True
  LtN ('ForkTree l n r) x =
    (If (Compare n x == 'LT && LtN l x == 'True && LtN r x == 'True)
      'True
      'False
    )

type family GtN (r :: Tree) (x :: Nat) :: Bool where
  GtN 'EmptyTree        x = 'True
  GtN ('ForkTree l n r) x =
    (If (Compare n x == 'GT && GtN l x == 'True && GtN r x == 'True)
      'True
      'False
    )

data BST :: Tree -> * where
  EmptyBST :: BST 'EmptyTree
  ForkBST  :: (LtN l n ~ 'True, GtN r n ~ 'True) =>
    BST l -> Natty n -> BST r -> BST ('ForkTree l n r)

instance Show (BST t) where
  show EmptyBST         = "E"
  show (ForkBST l n r)  = "F " ++ go l ++ " " ++ show n ++ " " ++ go r
    where
      go :: BST t' -> String
      go EmptyBST         = "E"
      go (ForkBST l' n' r')  = "(F " ++ go l' ++ " " ++ show n' ++ " " ++ go r' ++ ")"

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

-- | If LtN (ForkBST l n1 r) n :~: 'True, then
-- | LtN l n :~: 'True AND LtN r n :~: 'True
ltnSubTree :: BST t -> Natty n -> LtN t n :~: 'True
ltnSubTree EmptyBST         _ = Refl
ltnSubTree (ForkBST l n1 r) n = gcastWith (ltnNode n1 n) (gcastWith (ltnSubTree r n) (gcastWith (ltnSubTree l n) Refl))

-- | If LtN (ForkBST l n1 r) n :~: 'True, then
-- | LtN l n :~: 'True
ltnLeft :: BST l -> Natty n -> LtN l n :~: 'True
ltnLeft l n = case isEmpty l of
  E -> Refl
  NE -> gcastWith (ltnSubTree l n) Refl

-- | If LtN (ForkBST l n1 r) n :~: 'True, then
-- | Compare n1 n :~: 'LT
ltnNode :: Natty n1 -> Natty n -> Compare n1 n :~: 'LT
ltnNode n1 n = case owotoNat n1 n of
  EE -> undefined -- | Impossible case since Comapre n1 n :~: 'LT
  LE -> Refl
  GE -> undefined -- | Impossible case since Comapre n1 n :~: 'LT

-- | If LtN (ForkBST l n1 r) n :~: 'True, then
-- | LtN r n :~: 'True
ltnRight :: BST r -> Natty n -> LtN r n :~: 'True
ltnRight r n = case isEmpty r of
  E -> Refl
  NE -> gcastWith (ltnSubTree r n) Refl

-- | If Compare x n :~: 'LT AND (ForkBST t n _), then
-- | LtN (Insert x t) n :~: 'True
insertLeftProof :: (Compare x n ~ 'LT) =>
  Natty x -> BST t -> Natty n -> LtN (Insert x t) n :~: 'True
insertLeftProof _ EmptyBST         _ = Refl
insertLeftProof x (ForkBST l n1 r) n = case owotoNat x n1 of
  EE -> gcastWith (ltnRight r n) (gcastWith (ltnLeft l n) Refl)
  LE -> gcastWith (ltnNode n1 n) (gcastWith (ltnRight r n) (gcastWith (insertLeftProof x l n) Refl))
  GE -> gcastWith (ltnNode n1 n) (gcastWith (ltnLeft l n) (gcastWith (insertLeftProof x r n) Refl))

-- | If GtN (ForkBST l n1 r) n :~: 'True, then
-- | GtN l n :~: 'True AND GtN r n :~: 'True
gtnSubTree :: BST t -> Natty n -> GtN t n :~: 'True
gtnSubTree EmptyBST         _ = Refl
gtnSubTree (ForkBST l n1 r) n = gcastWith (gtnNode n1 n) (gcastWith (gtnSubTree r n) (gcastWith (gtnSubTree l n) Refl))

-- | If GtN (ForkBST l n1 r) n :~: 'True, then
-- | GtN l n :~: 'True
gtnLeft :: BST l -> Natty n -> GtN l n :~: 'True
gtnLeft l n = case isEmpty l of
  E -> Refl
  NE -> gcastWith (gtnSubTree l n) Refl

-- | If GtN (ForkBST l n1 r) n :~: 'True, then
-- | Compare n1 n :~: 'GT
gtnNode :: Natty n1 -> Natty n -> Compare n1 n :~: 'GT
gtnNode n1 n = case owotoNat n1 n of
  EE -> undefined -- | Impossible case since Compare n1 n :~: 'GT
  LE -> undefined -- | Impossible case since Compare n1 n :~: 'GT
  GE -> Refl

-- | If GtN (ForkBST l n1 r) n :~: 'True, then
-- | GtN r n :~: 'True
gtnRight :: BST r -> Natty n -> GtN r n :~: 'True
gtnRight r n = case isEmpty r of
  E -> Refl
  NE -> gcastWith (gtnSubTree r n) Refl

-- | If Compare x n :~: 'GT AND (ForkBST _ n t), then
-- | GtN n (Insert x t) :~: 'True
insertRightProof :: (Compare x n ~ 'GT) =>
  Natty x -> BST t -> Natty n -> GtN (Insert x t) n :~: 'True
insertRightProof _ EmptyBST         _ = Refl
insertRightProof x (ForkBST l n1 r) n = case owotoNat x n1 of
  EE -> gcastWith (gtnRight r n) (gcastWith (gtnLeft l n) Refl)
  LE -> gcastWith (gtnNode n1 n) (gcastWith (gtnRight r n) (gcastWith (insertRightProof x l n) Refl))
  GE -> gcastWith (gtnNode n1 n) (gcastWith (gtnLeft l n) (gcastWith (insertRightProof x r n) Refl))

insert :: Natty n -> BST t -> BST (Insert n t)
insert x EmptyBST         = ForkBST EmptyBST x EmptyBST
insert x (ForkBST l n r)  = case owotoNat x n of
  EE -> ForkBST l n r
  LE -> gcastWith (insertLeftProof x l n) (ForkBST (insert x l) n r)
  GE -> gcastWith (insertRightProof x r n) (ForkBST l n (insert x r))

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

-- | If x ~ Max t, then
-- | LtN (Delete x t) x :~: 'True
deleteProof :: (x ~ Max t) =>
  Natty x -> BST t -> LtN (Delete x t) x :~: 'True
deleteProof _ EmptyBST        = Refl
deleteProof x t@(ForkBST l n r) = case owotoNat x n of
  EE -> case isEmpty l of
    E -> gcastWith (ltnRight r n) Refl
    NE -> case isEmpty r of
      E -> Refl
      NE -> undefined -- | Impossible case since x ~ Max t
  LE -> undefined -- | Impossible case since can't happen x ~ Max t AND owoto x n = LE
  GE -> gcastWith (maxRightProof x t) (gcastWith (deleteProof x r) (gcastWith (ltnLeft l x) (gcastWith (ltnNode n x) Refl)))

-- | If x ~ Max ('ForkTree l n r), then
-- | x ~ Max r
maxRightProof :: (t ~ ('ForkTree l n r), Max t ~ x) =>
  Natty x -> BST t -> Max r :~: x
maxRightProof x (ForkBST _ _ r) = case isEmpty r of
  E  -> undefined -- | Impossible case since Max r must be x
  NE -> gcastWith (maxRightProof x r) Refl

-- | If x ~ Max l AND t ~ 'ForkTree l n r, then
-- | GtN x r :~: 'True
deleteProof' :: (x ~ Max l, t ~ 'ForkTree l n r) =>
  Natty x -> BST t -> GtN r x :~: 'True
deleteProof' x (ForkBST _ _ r) = gcastWith (gtnRight r x) Refl

-- | If Compare x n :~: 'LT AND (ForkTree t n _), then
-- | LtN (Delete x t) n :~: 'True
deleteLeftProof :: (Compare x n ~ 'LT) =>
  Natty x -> BST t -> Natty n -> LtN (Delete x t) n :~: 'True
deleteLeftProof _ EmptyBST         _ = Refl
deleteLeftProof x (ForkBST l n1 r) n = case owotoNat x n1 of
  EE -> case isEmpty l of
    E  -> gcastWith (ltnRight r n) Refl
    NE -> case isEmpty r of
      E  -> gcastWith (ltnLeft l n) Refl
      NE -> undefined -- | Impossible case since Compare x n :~: 'LT
  LE -> gcastWith (deleteLeftProof x l n) (gcastWith (ltnRight r n) (gcastWith (ltnNode n1 n) Refl))
  GE -> gcastWith (deleteLeftProof x r n) (gcastWith (ltnLeft l n) (gcastWith (ltnNode n1 n) Refl))

-- | If Compare x n :~: 'GT AND (ForkTree _ n t), then
-- | GtN n (Delete x t) :~: 'True
deleteRightProof :: (Compare x n ~ 'GT) =>
  Natty x -> BST t -> Natty n -> GtN (Delete x t) n :~: 'True
deleteRightProof _ EmptyBST         _ = Refl
deleteRightProof x (ForkBST l n1 r) n = case owotoNat x n1 of
  EE -> case isEmpty l of
    E  -> gcastWith (gtnRight r n) Refl
    NE -> case isEmpty r of
      E  -> gcastWith (gtnLeft l n) Refl
      NE -> undefined -- | Impossible case since Compare x n :~: 'GT
  LE -> gcastWith (deleteRightProof x l n) (gcastWith (gtnRight r n) (gcastWith (gtnNode n1 n) Refl))
  GE -> gcastWith (deleteRightProof x r n) (gcastWith (gtnLeft l n) (gcastWith (gtnNode n1 n) Refl))

delete :: Natty x -> BST t -> BST (Delete x t)
delete _ EmptyBST         = EmptyBST
delete x (ForkBST l n r)  = case owotoNat x n of
  EE -> case isEmpty l of
    E -> r
    NE -> case isEmpty r of
      E -> l
      NE -> gcastWith (deleteProof' maxL (ForkBST l n r)) (gcastWith (deleteProof maxL l) (ForkBST (delete maxL l) maxL r))
        where
          maxL = max l
  LE -> gcastWith (deleteLeftProof x l n) (ForkBST (delete x l) n r)
  GE -> gcastWith (deleteRightProof x r n) (ForkBST l n (delete x r))
