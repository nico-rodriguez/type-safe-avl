{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module BSTGeneral where

import Compare (Compare)
import Compare
import Data.Proxy
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

insertLeftProof :: (Compare x n ~ 'LT, LtN t n ~ 'True) =>
  s1 x -> BST t -> s2 n -> LtN (Insert x t) n :~: 'True
insertLeftProof _ _ _ = undefined

inserRightProof :: (Compare x n ~ 'GT, GtN n t ~ 'True) =>
  s1 x -> BST t -> s2 n -> GtN n (Insert x t) :~: 'True
inserRightProof _ _ _ = undefined

insert :: (Show (s x)) =>
  s x -> BST t -> BST (Insert x t)
insert x EmptyBST         = ForkBST EmptyBST x EmptyBST
insert x (ForkBST l n r)  = case owoto x n of
  EE -> ForkBST l n r
  LE -> gcastWith (insertLeftProof x l n) (ForkBST (insert x l) n r)
  GE -> gcastWith (inserRightProof x r n) (ForkBST l n (insert x r))

member :: s x -> BST t -> Bool
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

deleteProof :: (x ~ Max t) =>
  s1 x -> BST t -> LtN (Delete x t) x :~: 'True
deleteProof _ _ = undefined

deleteProof' :: (x ~ Max l, t ~ 'ForkTree l n r) =>
  s1 x -> BST t -> GtN x r :~: 'True
deleteProof' _ _ = undefined

deleteLeftProof :: (Compare x n ~ 'LT) =>
  s1 x -> BST t -> s2 n -> LtN (Delete x t) n :~: 'True
deleteLeftProof _ _ _ = undefined

deleteRightProof :: (Compare x n ~ 'GT) =>
  s1 x -> BST t -> s2 n -> GtN n (Delete x t) :~: 'True
deleteRightProof _ _ _ = undefined

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
