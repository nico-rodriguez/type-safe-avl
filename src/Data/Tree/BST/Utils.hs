{-|
Module      : Data.Tree.BST.Utils
Description : Miscellaneous type families
Copyright   : (c) Nicolás Rodríguez, 2021
License     : GPL-3
Maintainer  : Nicolás Rodríguez
Stability   : experimental
Portability : POSIX

Type level search for a key and a value type in a type tree.
-}

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE Safe                 #-}

module Data.Tree.BST.Utils (
  Member,
  LookupValueType
) where

import           Data.Kind          (Type)
import           Data.Tree.ITree    (Tree (EmptyTree, ForkTree))
import           Data.Tree.Node     (Node)
import           Data.Type.Bool     (If)
import           Data.Type.Equality (type (==))
import           GHC.TypeLits       (TypeError, ErrorMessage(Text, ShowType, (:<>:)))
import           GHC.TypeNats       (CmpNat, Nat)
import           Prelude            (Bool (True), Ordering (EQ, LT))


-- | Type family to test wether there is a node in the tree @t@ with key @x@.
-- | It assumes that @t@ is a BST in order to perform the search.
type family Member (x :: Nat) (t :: Tree) (t' :: Tree) :: Bool where
  Member x ('ForkTree l (Node n _a) r) t' =
    (If (CmpNat x n == 'EQ)
      'True
      (If (CmpNat x n == 'LT)
        (Member x l t')
        (Member x r t')
      )
    )
  Member x 'EmptyTree t' = TypeError ('Text "Key " ':<>: 'ShowType x ':<>: 'Text " not found in tree " ':<>: 'ShowType t')

-- | Type family to search for the type of the value stored with key @x@ in a tree @t@.
-- | It assumes that @t@ is a BST and that @x@ is a member of @t@ in order to perform the search
-- | (so it always return a valid type).
type family LookupValueType (x :: Nat) (t :: Tree) (t' :: Tree) :: Type where
  LookupValueType x ('ForkTree l (Node n a) r) t' =
    (If (CmpNat x n == 'EQ)
      a
      (If (CmpNat x n == 'LT)
        (LookupValueType x l t')
        (LookupValueType x r t')
      )
    )
  LookupValueType x 'EmptyTree t' = TypeError ('Text "Key " ':<>: 'ShowType x ':<>: 'Text " not found in tree " ':<>: 'ShowType t')
