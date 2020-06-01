{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Tree.BST.Invariants (
  LtN, GtN, IsBST
) where

import           Data.Tree.ITree    (Tree (EmptyTree, ForkTree))
import           Data.Tree.Node     (Node)
import           Data.Type.Bool     (type (&&))
import           Data.Type.Equality (type (==))
import           GHC.TypeNats       (CmpNat, Nat)
import           Prelude            (Bool (True), Ordering (GT, LT))


-- | Check if all elements of the tree are strictly less than x
type family LtN (l :: Tree) (x :: Nat) :: Bool where
  LtN 'EmptyTree                 _x = 'True
  LtN ('ForkTree l (Node n _a) r) x = CmpNat n x == 'LT && LtN l x && LtN r x

-- | Check if all elements of the tree are strictly greater than x
type family GtN (r :: Tree) (x :: Nat) :: Bool where
  GtN 'EmptyTree                 _x = 'True
  GtN ('ForkTree l (Node n _a) r) x = CmpNat n x == 'GT && GtN l x && GtN r x

-- | Check if tree is BST
type family IsBST (t :: Tree) :: Bool where
  IsBST 'EmptyTree                  = 'True
  IsBST ('ForkTree l (Node n _a) r) = IsBST l && IsBST r && LtN l n && GtN r n
