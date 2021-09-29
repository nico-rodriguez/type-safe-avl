{-|
Module      : Data.Tree.BST.Invariants
Description : Type level BST invariants
Copyright   : (c) Nicolás Rodríguez, 2021
License     : GPL-3
Maintainer  : Nicolás Rodríguez
Stability   : experimental
Portability : POSIX

Type level restrictions for the key ordering in type safe BST trees.
-}

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE Safe                 #-}

module Data.Tree.BST.Invariants (
  LtN, GtN
) where

import           Data.Tree.ITree    (Tree (EmptyTree, ForkTree))
import           Data.Tree.Node     (Node)
import           Data.Type.Bool     (type (&&))
import           Data.Type.Equality (type (==))
import           GHC.TypeNats       (CmpNat, Nat)
import           Prelude            (Bool (True), Ordering (GT, LT))


-- | Check if all elements of the tree `l` are strictly less than `x`.
type family LtN (l :: Tree) (x :: Nat) :: Bool where
  LtN 'EmptyTree                 _x = 'True
  LtN ('ForkTree l (Node n _a) r) x = CmpNat n x == 'LT && LtN l x && LtN r x


-- | Check if all elements of the tree `r` are strictly greater than `x`.
type family GtN (r :: Tree) (x :: Nat) :: Bool where
  GtN 'EmptyTree                 _x = 'True
  GtN ('ForkTree l (Node n _a) r) x = CmpNat n x == 'GT && GtN l x && GtN r x
