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

module BST (Nat(..), Natty(..), BST(..), Join, insert) where--,
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

-- Type class for ordering Natural Numbers
class LtN (m :: Nat) (n :: Nat) where
instance            LtN 'Z     ('S n) where
instance LtN m n => LtN ('S m) ('S n) where

data OWOTO :: Nat -> Nat -> * where
  LE :: (LtN x y, Compare x y ~ 'LT)  => OWOTO x y
  EE :: (Compare x x ~ 'EQ)           => OWOTO x x
  GE :: (LtN y x, Compare x y ~ 'GT)  => OWOTO x y

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

type family Join (e1 :: [Nat]) (n :: Nat) (e2 :: [Nat]) :: [Nat] where
  Join '[] n ys = (n:ys)
  Join (x:xs) n ys = x:Join xs n ys

type family Compare (m :: Nat) (n :: Nat) :: Ordering where
  Compare 'Z      'Z      = 'EQ
  Compare ('S m)  ('S n)  = Compare m n
  Compare ('S m)  'Z      = 'GT
  Compare 'Z      ('S n)  = 'LT

type family Insert (n :: Nat) (l :: [Nat]) :: [Nat] where
  Insert n '[] = '[n]
  Insert n (x:xs) =
    If (Compare n x == 'GT)
      (x:Insert n xs)
      (If (Compare n x == 'LT)
        (n:x:xs)
        (x:xs)  -- Compare n x == 'EQ
      )

joinInsertLT :: (Compare n m ~ 'LT) => Natty n -> BST l -> Natty m -> BST r ->
  Join (Insert n l) m r :~: Insert n (Join l m r)
joinInsertLT n EmptyBST m r = Refl

joinInsertEQ :: (Compare n m ~ 'EQ) => Natty n -> BST l -> Natty m -> BST r ->
  Insert n (Join l m r) :~: Join l m r
joinInsertEQ n EmptyBST m r = Refl

joinInsertGT :: (Compare n m ~ 'GT) => Natty n -> BST l -> Natty m -> BST r ->
  Join l m (Insert n r) :~: Insert n (Join l m r)
joinInsertGT n EmptyBST m r = Refl

-- Insert a Singleton Nat (Natty) into a BST.
insert :: Natty n -> BST l -> BST (Insert n l)
insert n EmptyBST = RootBST EmptyBST n EmptyBST
insert n (RootBST l m r) = case owoto n m of
  LE -> gcastWith (joinInsertLT n l m r) (RootBST (insert n l) m r)
  EE -> gcastWith (joinInsertEQ n l m r) (RootBST l m r)
  GE -> gcastWith (joinInsertGT n l m r) (RootBST l m (insert n r))
