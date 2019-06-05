{-# LANGUAGE DataKinds #-}

{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE GADTs #-}

{-# LANGUAGE KindSignatures #-}

{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE PolyKinds #-}

{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE UndecidableInstances #-}

module BST (Nat(..), Natty(..), BST(..)) where--,
  --isEmpty, member, insert, preorder, inorder, postorder, outorder) where

import Data.Type.Bool
import Data.Type.Equality
import Prelude hiding (min)

-- Natural Numbers.
data Nat = Z | S Nat
  deriving (Eq, Ord, Show)

type family (m :: Nat) :+ (n :: Nat) :: Nat where
  'Z   :+ n = n
  'S m :+ n = 'S (m :+ n)

-- Singleton for Natural Numbers.
data Natty :: Nat -> * where
  Zy :: Natty 'Z
  Sy :: Natty n -> Natty ('S n)
deriving instance Eq (Natty n)
deriving instance Show (Natty n)

natty2Nat :: Natty n -> Nat
natty2Nat Zy      = Z
natty2Nat (Sy n)  = S m
  where
    m = natty2Nat n

-- Type class for ordering Natural Numbers
class LtN (m :: Nat) (n :: Nat) where
instance            LtN 'Z     ('S n) where
instance LtN m n => LtN ('S m) ('S n) where

data OWOTO :: Nat -> Nat -> * where
  LE :: LtN x y => OWOTO x y
  GE :: LtN y x => OWOTO x y
  EE ::            OWOTO x x

owoto :: Natty m -> Natty n -> OWOTO m n
owoto Zy      Zy      = EE
owoto Zy      (Sy _)  = LE
owoto (Sy _)  Zy      = GE
owoto (Sy m)  (Sy n)  = case owoto m n of
  LE -> LE
  GE -> GE
  EE -> EE

-- Binary Search Tree of Singleton Natural Numbers.
data BST :: [Nat] -> * where
  EmptyBST :: BST '[]
  RootBST  :: BST l1 -> Natty n -> BST l2 -> BST (Join l1 n l2)
deriving instance Show (BST l)

-- Emptiness can be check with types.
isEmpty :: BST l -> Bool
isEmpty EmptyBST = True
isEmpty _        = False

-- Member can be check with types.
member :: BST l -> Natty n -> Bool
member EmptyBST         _ = False
member (RootBST l m r)  x = case owoto m x of
  LE -> member r x
  EE -> True
  GE -> member l x

type family Join (e1 :: [Nat]) (n :: Nat) (e2 :: [Nat]) :: [Nat] where
  Join '[] n ys = (n:ys)
  Join (x:xs) n ys = x:Join xs n ys

type family Compare (m :: Nat) (n :: Nat) :: Ordering where
  Compare 'Z      'Z      = 'EQ
  Compare ('S m)  ('S n)  = Compare m n
  Compare ('S m)  'Z      = 'GT
  Compare 'Z      ('S n)  = 'LT

type family Member (n :: Nat) (l :: [Nat]) :: Bool where
  Member n (x:xs) =
    If (Compare n x == 'LT)
      'False
      (If (Compare n x == 'GT)
        (Member n xs)
        'True   -- Compare n x == 'EQ
      )

type family Insert (n :: Nat) (l :: [Nat]) :: [Nat] where
  Insert n '[] = '[n]
  Insert n (x:xs) =
    If (Compare n x == 'GT)
      (x:Insert n xs)
      (If (Compare n x == 'LT)
        (n:x:xs)
        (x:xs)  -- Compare n x == 'EQ
      )

type family Delete (n :: Nat) (l :: [Nat]) :: [Nat] where
  Delete n '[] = '[]
  Delete n (x:xs) =
    If (Compare n x == 'EQ)
      xs
      (If (Compare n x == 'GT)
        (x:Delete n xs)
        (x:xs)  -- Compare n x == 'LT
      )

insertLT :: (Join l m r ~ p, Insert n p ~ Join (Insert n l) m r) =>
  Natty n -> BST l -> Natty m -> BST r -> BST p -> t -> t
insertLT Zy     left node right previousBST newBST = newBST
insertLT (Sy n) left node right previousBST newBST = newBST

insertEQ :: Natty n -> BST l -> Natty n -> BST r -> BST p ->
  ((p ~ Insert n p) => t) -> t
insertEQ Zy left Zy right previousBST newBST = newBST
insertEQ (Sy n) left (Sy m) right previousBST newBST
  = insertEQ n left m right previousBST newBST

-- Insert a Singleton Nat (Natty) into a BST.
insert :: Natty n -> BST l -> BST (Insert n l)
insert n EmptyBST = RootBST EmptyBST n EmptyBST
insert n (RootBST l m r) = case owoto n m of
  LE -> insertLT n l m r (RootBST l m r) (RootBST (insert n l) m r)
  EE -> insertEQ n l m r (RootBST l m r) (RootBST l m r)
  GE -> RootBST l m (insert n r)

min :: BST (n:ns) -> Natty n
min (RootBST l m _) = if isEmpty l
                      then m
                      else min l

delete :: Natty n -> BST l -> BST (Delete n l)
delete _ EmptyBST = EmptyBST
delete n (RootBST l m r) = case owoto n m of
  LE -> RootBST (delete n l) m r
  EE -> case isEmpty r of
          True  -> l
          False -> RootBST l m1 r1
            where
              m1 = min r
              r1 = delete m1 r
  GE -> RootBST l m (delete n r)

preorder :: BST l -> [Nat]
preorder EmptyBST         = []
preorder (RootBST l m r)  = (natty2Nat m : preorder l) ++ preorder r

inorder :: BST l -> [Nat]
inorder EmptyBST        = []
inorder (RootBST l m r) = inorder l ++ [natty2Nat m] ++ inorder r

outorder :: BST l -> [Nat]
outorder EmptyBST        = []
outorder (RootBST l m r) = outorder r ++ [natty2Nat m] ++ outorder l

postorder :: BST l -> [Nat]
postorder EmptyBST        = []
postorder (RootBST l m r) = postorder l ++ postorder r ++ [natty2Nat m]
