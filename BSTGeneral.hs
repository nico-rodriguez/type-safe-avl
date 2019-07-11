{-# LANGUAGE DataKinds #-}

{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE GADTs #-}

{-# LANGUAGE PolyKinds #-}

{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE UndecidableInstances #-}

module BST where

-- import Compare
import Data.Type.Bool
import Data.Type.Equality
-- import Prelude hiding (max)
import Nat

data Tree n :: * where
  EmptyTree :: Tree n
  ForkTree  :: Tree n -> n -> Tree n -> Tree n

type family LtN (l :: Tree n) (a :: n) :: Bool where
  LtN 'EmptyTree        a = 'True
  LtN ('ForkTree l n r) a =
    (If (Compare n a == 'LT)
      (If (LtN l a == 'True)
        (If (LtN r a == 'True)
          'True
          'False
        )
        'False
      )
      'False
    )

type family GtN (a :: n) (r :: Tree n) :: Bool where
  GtN a 'EmptyTree        = 'True
  GtN a ('ForkTree l n r) =
    (If (Compare n a == 'GT)
      (If (GtN a l == 'True)
        (If (GtN a r == 'True)
          'True
          'False
        )
        'False
      )
      'False
    )

data BST :: Tree n -> * where
  EmptyBST :: BST 'EmptyTree
  ForkBST  :: (Show (s n), LtN l n ~ 'True, GtN n r ~ 'True) => BST l -> s n -> BST r -> BST ('ForkTree l n r)

instance Show (BST t) where
  show EmptyBST         = "E"
  show (ForkBST l sn r) = "F " ++ go l ++ " " ++ show sn ++ " " ++ go r
    where
      go :: BST t' -> String
      go EmptyBST         = "E"
      go (ForkBST l' sn' r')  = "(" ++ go l' ++ " " ++ show sn' ++ " " ++ go r' ++ ")"

type family Insert (n :: a) (t :: Tree a) :: Tree a where
  Insert n 'EmptyTree         = 'ForkTree 'EmptyTree n 'EmptyTree
  Insert n ('ForkTree l m r)  =
    (If (Compare n m == 'EQ)
      ('ForkTree l m r)
      (If (Compare n m == 'LT)
        ('ForkTree (Insert n l) m r)
        ('ForkTree l m (Insert n r))
      )
    )

-- insert :: s n -> BST t -> BST (Insert n t)
-- insert n EmptyBST         = ForkBST EmptyBST n EmptyBST
-- insert n (ForkBST l m r)  = case owoto n m of
--   EE -> ForkBST l m r
--   LE -> ForkBST (insert n l) m r
--   GE -> ForkBST l m (insert n r)

type family Member (n :: a) (t :: Tree a) :: Bool where
  Member n 'EmptyTree         = 'False
  Member n ('ForkTree l m r)  =
    (If (Compare n m == 'EQ)
      'True
      (If (Compare n m == 'LT)
        (Member n l)
        (Member n r)
      )
    )

-- member :: sa -> BST t -> Bool
-- member _ EmptyBST         = False
-- member n (ForkBST l m r)  = case owoto n m of
--   EE -> True
--   LE -> member n l
--   GE -> member n r

type family IsEmpty (t :: Tree a) :: Bool where
  IsEmpty 'EmptyTree        = 'True
  IsEmpty ('ForkTree l m r) = 'False

data IET :: Tree * -> * where
  E   :: IET 'EmptyTree
  NE  :: IET ('ForkTree l m r)

-- isEmpty :: BST n t -> IET t
-- isEmpty EmptyBST  = E
-- isEmpty ForkBST{} = NE

type family Max (t :: Tree a) :: a where
  Max ('ForkTree l m r) =
    (If (IsEmpty r == 'True)
      m
      (Max r)
    )

-- max :: BST ('ForkTree l m r) ->  sa (Max ('ForkTree l m r))
-- max (ForkBST _ n r) = case isEmpty r of
  -- E -> n
  -- NE -> max r

type family Delete (n :: a) (t :: Tree a) :: Tree a where
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

-- delete :: sa -> BST t -> BST (Delete sa t)
-- delete _ EmptyBST         = EmptyBST
-- delete n (ForkBST l m r)  = case owoto n m of
--   EE -> case isEmpty l of
--     E -> r
--     NE -> case isEmpty r of
--       E -> l
--       NE -> ForkBST (delete maxL l) maxL r
--         where
--           maxL = max l
--   LE -> ForkBST (delete n l) m r
--   GE -> ForkBST l m (delete n r)

sbn0 :: Natty 'Z
sbn0 = Zy

sbn1 :: Natty ('S 'Z)
sbn1 = Sy Zy

sbn2 :: Natty ('S ('S 'Z))
sbn2 = Sy (Sy Zy)

t0 = ForkTree EmptyTree Z EmptyTree

t1 = ForkBST EmptyBST sbn0 EmptyBST

t2 = ForkBST t1 sbn1 EmptyBST

t3 = ForkBST t1 sbn1 (ForkBST EmptyBST sbn2 EmptyBST)
