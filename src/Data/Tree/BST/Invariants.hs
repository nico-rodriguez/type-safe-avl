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
import           Data.Type.Bool     (type (&&), If)
import           Data.Type.Equality (type (==))
import           GHC.TypeLits       (TypeError, ErrorMessage(Text, ShowType, (:<>:)))
import           GHC.TypeNats       (CmpNat, Nat)
import           Prelude            (Bool (True), Ordering (GT, LT))


-- | Check if all elements of the tree `l` are strictly less than `x`.
type family LtN (l :: Tree) (x :: Nat) :: Bool where
  LtN 'EmptyTree                 _x = 'True
  LtN ('ForkTree l (Node n a) r) x =
    (If (CmpNat n x == 'LT)
      (If (LtN l x)
        (If (LtN r x)
          'True
          (TypeError (Text "Key " :<>: ShowType x :<>: Text " is not strictly lesser than keys in tree " :<>: ShowType r))
        )
        (TypeError (Text "Key " :<>: ShowType x :<>: Text " is not strictly lesser than keys in tree " :<>: ShowType l))
      )
      (TypeError (Text "Key " :<>: ShowType x :<>: Text " is not lesser than key " :<>: ShowType n))
    )


-- | Check if all elements of the tree `r` are strictly greater than `x`.
type family GtN (r :: Tree) (x :: Nat) :: Bool where
  GtN 'EmptyTree                 _x = 'True
  GtN ('ForkTree l (Node n _a) r) x = 
    (If (CmpNat n x == 'GT)
      (If (GtN l x)
        (If (GtN r x)
          'True
          (TypeError (Text "Key " :<>: ShowType x :<>: Text " is not strictly greater than keys in tree " :<>: ShowType r))
        )
        (TypeError (Text "Key " :<>: ShowType x :<>: Text " is not strictly greater than keys in tree " :<>: ShowType l))
      )
      (TypeError (Text "Key " :<>: ShowType x :<>: Text " is not greater than key " :<>: ShowType n))
    )
