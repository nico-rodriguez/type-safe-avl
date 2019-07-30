{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module BSTGeneral where

-- import Compare (Compare, OWOTO(..), Owoto(..), owoto)
import Compare
import Data.Type.Bool
import Data.Type.Equality
import Prelude hiding (max)
-- import Nat

data Tree a :: * where
  EmptyTree :: Tree a
  ForkTree  :: Tree a -> a -> Tree a -> Tree a

type family LtN (l :: Tree a) (x :: a) :: Bool where
  LtN 'EmptyTree        x = 'True
  LtN ('ForkTree l n r) x =
    (If (Compare n x == 'LT && LtN l x == 'True && LtN r x == 'True)
      'True
      'False
    )

type family GtN (x :: a) (r :: Tree a) :: Bool where
  GtN x 'EmptyTree        = 'True
  GtN x ('ForkTree l n r) =
    (If (Compare n x == 'GT && GtN x l == 'True && GtN x r == 'True)
      'True
      'False
    )

type family IsBST (t :: Tree a) :: Bool where
  IsBST 'EmptyTree = 'True
  IsBST ('ForkTree 'EmptyTree n 'EmptyTree) = 'True
  IsBST ('ForkTree 'EmptyTree n ('ForkTree rl rn rr)) =
    (If (Compare rn n == 'GT && IsBST ('ForkTree rl rn rr) == 'True)
      'True
      'False
    )
  IsBST ('ForkTree ('ForkTree ll ln lr) n 'EmptyTree) =
    (If (Compare ln n == 'LT && IsBST ('ForkTree ll ln lr) == 'True)
      'True
      'False
    )
  IsBST ('ForkTree ('ForkTree ll ln lr) n ('ForkTree rl rn rr)) =
    (If (Compare ln n == 'LT && Compare rn n == 'GT && IsBST ('ForkTree ll ln lr) == 'True && IsBST ('ForkTree rl rn rr) == 'True)
      'True
      'False
    )

data BST :: Tree a -> * where
  EmptyBST :: BST 'EmptyTree
  -- ForkBST  :: (Show (s n), LtN l n ~ 'True, GtN n r ~ 'True) =>
  ForkBST  :: (LtN l n ~ 'True, GtN n r ~ 'True) =>
    BST l -> s n -> BST r -> BST ('ForkTree l n r)

-- instance Show (BST t) where
--   show EmptyBST         = "E"
--   show (ForkBST l sn r) = "F " ++ go l ++ " " ++ show sn ++ " " ++ go r
--     where
--       go :: BST t' -> String
--       go EmptyBST         = "E"
--       go (ForkBST l' sn' r')  = "(" ++ go l' ++ " " ++ show sn' ++ " " ++ go r' ++ ")"

type family Insert (x :: a) (t :: Tree a) :: Tree a where
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
ltnSubTree :: BST t -> s n -> LtN t n :~: 'True
ltnSubTree EmptyBST         _ = Refl
ltnSubTree (ForkBST l n1 r) n = gcastWith (ltnNode n1 n) (gcastWith (ltnSubTree r n) (gcastWith (ltnSubTree l n) Refl))

-- | If LtN (ForkBST l n1 r) n :~: 'True, then
-- | LtN l n :~: 'True
ltnLeft :: BST l -> s n -> LtN l n :~: 'True
ltnLeft l n = case isEmpty l of
  E  -> Refl
  NE -> gcastWith (ltnSubTree l n) Refl

-- | If LtN (ForkBST l n1 r) n :~: 'True, then
-- | Compare n1 n :~: 'LT
ltnNode :: s1 n1 -> s2 n -> Compare n1 n :~: 'LT
ltnNode n1 n = case owoto n1 n of
  EE -> undefined -- | Impossible case since Comapre n1 n :~: 'LT
  LE -> Refl
  GE -> undefined -- | Impossible case since Comapre n1 n :~: 'LT

-- | If LtN (ForkBST l n1 r) n :~: 'True, then
-- | LtN r n :~: 'True
ltnRight :: BST r -> s n -> LtN r n :~: 'True
ltnRight r n = case isEmpty r of
  E  -> Refl
  NE -> gcastWith (ltnSubTree r n) Refl

-- | If Compare x n :~: 'LT AND (ForkBST t n _), then
-- | LtN (Insert x t) n :~: 'True
insertLeftProof :: (Compare x n ~ 'LT) =>
  s1 x -> BST t -> s2 n -> LtN (Insert x t) n :~: 'True
insertLeftProof _ EmptyBST         _ = Refl
insertLeftProof x (ForkBST l n1 r) n = case owoto x n1 of
  EE -> gcastWith (ltnRight r n) (gcastWith (ltnLeft l n) Refl)
  LE -> gcastWith (ltnNode n1 n) (gcastWith (ltnRight r n) (gcastWith (insertLeftProof x l n) Refl))
  GE -> gcastWith (ltnNode n1 n) (gcastWith (ltnLeft l n) (gcastWith (insertLeftProof x r n) Refl))

-- | If GtN (ForkBST l n1 r) n :~: 'True, then
-- | GtN l n :~: 'True AND GtN r n :~: 'True
gtnSubTree :: BST t -> s n -> GtN n t :~: 'True
gtnSubTree EmptyBST         _ = Refl
gtnSubTree (ForkBST l n1 r) n = gcastWith (gtnNode n1 n) (gcastWith (gtnSubTree r n) (gcastWith (gtnSubTree l n) Refl))

-- | If GtN (ForkBST l n1 r) n :~: 'True, then
-- | GtN l n :~: 'True
gtnLeft :: BST l -> s n -> GtN n l :~: 'True
gtnLeft l n = case isEmpty l of
  E  -> Refl
  NE -> gcastWith (gtnSubTree l n) Refl

-- | If GtN (ForkBST l n1 r) n :~: 'True, then
-- | Compare n1 n :~: 'GT
gtnNode :: s1 n1 -> s2 n -> Compare n1 n :~: 'GT
gtnNode n1 n = case owoto n1 n of
  EE -> undefined -- | Impossible case since Compare n1 n :~: 'GT
  LE -> undefined -- | Impossible case since Compare n1 n :~: 'GT
  GE -> Refl

-- | If GtN (ForkBST l n1 r) n :~: 'True, then
-- | GtN r n :~: 'True
gtnRight :: BST r -> s n -> GtN n r :~: 'True
gtnRight r n = case isEmpty r of
  E -> Refl
  NE -> gcastWith (gtnSubTree r n) Refl

-- | If Compare x n :~: 'GT AND (ForkBST _ n t), then
-- | GtN n (Insert x t) :~: 'True
insertRightProof :: (Compare x n ~ 'GT) =>
  s1 x -> BST t -> s2 n -> GtN n (Insert x t) :~: 'True
insertRightProof _ EmptyBST         _ = Refl
insertRightProof x (ForkBST l n1 r) n = case owoto x n1 of
  EE -> gcastWith (gtnRight r n) (gcastWith (gtnLeft l n) Refl)
  LE -> gcastWith (gtnNode n1 n) (gcastWith (gtnRight r n) (gcastWith (insertRightProof x l n) Refl))
  GE -> gcastWith (gtnNode n1 n) (gcastWith (gtnLeft l n) (gcastWith (insertRightProof x r n) Refl))

insert :: (Show (s x)) =>
  s x -> BST t -> BST (Insert x t)
insert x EmptyBST         = ForkBST EmptyBST x EmptyBST
insert x (ForkBST l n r)  = case owoto x n of
  EE -> ForkBST l n r
  LE -> gcastWith (insertLeftProof x l n) (ForkBST (insert x l) n r)
  GE -> gcastWith (insertRightProof x r n) (ForkBST l n (insert x r))

member :: (Owoto s) =>
  s x -> BST t -> Bool
member _ EmptyBST         = False
member x (ForkBST l n r)  = case owoto x n of
  EE -> True
  LE -> member x l
  GE -> member x r

type family IsEmpty (t :: Tree a) :: Bool where
  IsEmpty 'EmptyTree        = 'True
  IsEmpty ('ForkTree l n r) = 'False

data IET :: Tree a -> * where
  E   :: IET 'EmptyTree
  NE  :: IET ('ForkTree l n r)

isEmpty :: BST t -> IET t
isEmpty EmptyBST  = E
isEmpty ForkBST{} = NE

type family Max (t :: Tree a) :: a where
  Max ('ForkTree l n r) =
    (If (IsEmpty r == 'True)
      n
      (Max r)
    )

max :: BST ('ForkTree l n r) ->  s (Max ('ForkTree l n r))
max (ForkBST _ n r) = case isEmpty r of
  E -> n
  NE -> max r

type family Delete (x :: a) (t :: Tree a) :: Tree a where
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
  s1 x -> BST t -> LtN (Delete x t) x :~: 'True
deleteProof _ EmptyBST        = Refl
deleteProof x t@(ForkBST l n r) = case owoto x n of
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
  s1 x -> BST t -> Max r :~: x
maxRightProof x (ForkBST _ _ r) = case isEmpty r of
  E  -> undefined -- | Impossible case since Max r must be x
  NE -> gcastWith (maxRightProof x r) Refl

-- | If x ~ Max l AND t ~ 'ForkTree l n r, then
-- | GtN x r :~: 'True
deleteProof' :: (x ~ Max l, t ~ 'ForkTree l n r) =>
  s1 x -> BST t -> GtN x r :~: 'True
deleteProof' x (ForkBST _ _ r) = gcastWith (gtnRight r x) Refl

-- | If Compare x n :~: 'LT AND (ForkTree t n _), then
-- | LtN (Delete x t) n :~: 'True
deleteLeftProof :: (Compare x n ~ 'LT) =>
  s1 x -> BST t -> s2 n -> LtN (Delete x t) n :~: 'True
deleteLeftProof _ EmptyBST         _ = Refl
deleteLeftProof x (ForkBST l n1 r) n = case owoto x n1 of
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
  s1 x -> BST t -> s2 n -> GtN n (Delete x t) :~: 'True
deleteRightProof _ EmptyBST         _ = Refl
deleteRightProof x (ForkBST l n1 r) n = case owoto x n1 of
  EE -> case isEmpty l of
    E  -> gcastWith (gtnRight r n) Refl
    NE -> case isEmpty r of
      E  -> gcastWith (gtnLeft l n) Refl
      NE -> undefined -- | Impossible case since Compare x n :~: 'GT
  LE -> gcastWith (deleteRightProof x l n) (gcastWith (gtnRight r n) (gcastWith (gtnNode n1 n) Refl))
  GE -> gcastWith (deleteRightProof x r n) (gcastWith (gtnLeft l n) (gcastWith (gtnNode n1 n) Refl))

delete :: s x -> BST t -> BST (Delete x t)
delete _ EmptyBST         = EmptyBST
delete x (ForkBST l n r)  = case owoto x n of
  EE -> case isEmpty l of
    E -> r
    NE -> case isEmpty r of
      E -> l
      NE -> gcastWith (deleteProof' maxL (ForkBST l n r)) (gcastWith (deleteProof maxL l) (ForkBST (delete maxL l) maxL r))
        where
          maxL = max l
  LE -> gcastWith (deleteLeftProof x l n) (ForkBST (delete x l) n r)
  GE -> gcastWith (deleteRightProof x r n) (ForkBST l n (delete x r))

-- sbn0 :: Natty 'Z
-- sbn0 = Zy
--
-- sbn1 :: Natty ('S 'Z)
-- sbn1 = Sy Zy
--
-- sbn2 :: Natty ('S ('S 'Z))
-- sbn2 = Sy (Sy Zy)
--
-- t0 = ForkTree EmptyTree Z EmptyTree
--
-- t1 = ForkBST EmptyBST sbn0 EmptyBST
--
-- t2 = ForkBST t1 sbn1 EmptyBST
--
-- t3 = ForkBST t1 sbn1 (ForkBST EmptyBST sbn2 EmptyBST)
