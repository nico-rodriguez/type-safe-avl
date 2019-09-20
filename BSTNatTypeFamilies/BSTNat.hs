{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module BSTNatTypeFamilies.BSTNat where

import           BSTNatTypeFamilies.Nat (Compare, Nat, Natty, OWOTO (..),
                                         owotoNat)
import           Data.Type.Bool
import           Data.Type.Equality
import           Prelude                hiding (max)


data Tree :: * where
  EmptyTree :: Tree
  ForkTree  :: Tree -> Nat -> Tree -> Tree

-- | Check if all elements of the tree are strictly less than x
type family LtN (l :: Tree) (x :: Nat) :: Bool where
  LtN 'EmptyTree        x = 'True
  LtN ('ForkTree l n r) x =
    (If (Compare n x == 'LT && LtN l x == 'True && LtN r x == 'True)
      'True
      'False
    )

-- | Check if all elements of the tree are strictly greater than x
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

-- | Proofs of general facts.
-- | Prove that n1 < n. This function is applied in a context where (Compare n1 n ~ 'LT) actually holds
proofLtNNode :: Natty n1 -> Natty n -> Compare n1 n :~: 'LT
proofLtNNode n1 n = case owotoNat n1 n of
  LE -> Refl

-- | Prove that n1 > n. This function is applied in a context where (Compare n1 n ~ 'GT) actually holds
proofGtNNode :: Natty n1 -> Natty n -> Compare n1 n :~: 'GT
proofGtNNode n1 n = case owotoNat n1 n of
  GE -> Refl

-- | Prove that LtN t n ~ 'True. This function is applied in a context where (LtN t n ~ 'True) actually holds
proofLtNTree :: BST t -> Natty n -> LtN t n :~: 'True
proofLtNTree EmptyBST         _ = Refl
proofLtNTree (ForkBST l n1 r) n = gcastWith (proofLtNNode n1 n) (gcastWith (proofLtNTree r n) (gcastWith (proofLtNTree l n) Refl))

-- | Prove that GtN t n ~ 'True. This function is applied in a context where (GtN t n ~ 'True) actually holds
proofGtNTree :: BST t -> Natty n -> GtN t n :~: 'True
proofGtNTree EmptyBST         _ = Refl
proofGtNTree (ForkBST l n1 r) n = gcastWith (proofGtNNode n1 n) (gcastWith (proofGtNTree r n) (gcastWith (proofGtNTree l n) Refl))

-- | Proofs for insert. Prove that insertion preserves the invariants of BST.

-- | Prove that if LtN t n ~ 'True, then left sub tree l of t verifies LtN l n
proofLtNLeft :: (t ~ 'ForkTree l n1 r, LtN t n ~ 'True) =>
  BST t -> Natty n -> LtN l n :~: 'True
proofLtNLeft (ForkBST l _ _) n = gcastWith (proofLtNTree l n) Refl

-- | Prove that if LtN t n ~ 'True, then right sub tree r of t verifies LtN r n
proofLtNRight :: (t ~ 'ForkTree l n1 r, LtN t n ~ 'True) =>
  BST t -> Natty n -> LtN r n :~: 'True
proofLtNRight (ForkBST _ _ r) n = gcastWith (proofLtNTree r n) Refl

-- | Prove that if GtN t n ~ 'True, then left sub tree l of t verifies GtN l n
proofGtNLeft :: (t ~ 'ForkTree l n1 r, GtN t n ~ 'True) =>
  BST t -> Natty n -> GtN l n :~: 'True
proofGtNLeft (ForkBST l _ _) n = gcastWith (proofGtNTree l n) Refl

-- | Prove that if GtN t n ~ 'True, then right sub tree r of t verifies GtN r n
proofGtNRight :: (t ~ 'ForkTree l n1 r, GtN t n ~ 'True) =>
  BST t -> Natty n -> GtN r n :~: 'True
proofGtNRight (ForkBST _ _ r) n = gcastWith (proofGtNTree r n) Refl

-- | If Compare x n :~: 'LT AND (ForkBST t n _), then
-- | LtN (Insert x t) n :~: 'True
proofLtNInsert :: (Compare x n ~ 'LT, LtN t n ~ 'True) =>
  Natty x -> BST t -> Natty n -> LtN (Insert x t) n :~: 'True
proofLtNInsert _ EmptyBST         _ = Refl
proofLtNInsert x t@(ForkBST l n1 r) n = case owotoNat x n1 of
  EE -> Refl
  LE -> gcastWith (proofLtNLeft t n) (gcastWith (proofLtNInsert x l n) (gcastWith (proofLtNInsert x l n1) (gcastWith (proofLtNRight t n) Refl)))
  GE -> gcastWith (proofLtNRight t n) (gcastWith (proofLtNInsert x r n) (gcastWith (proofLtNLeft t n) Refl))

-- | If Compare x n :~: 'GT AND (ForkBST _ n t), then
-- | GtN n (Insert x t) :~: 'True
proofGtNInsert :: (Compare x n ~ 'GT, GtN t n ~ 'True) =>
  Natty x -> BST t -> Natty n -> GtN (Insert x t) n :~: 'True
proofGtNInsert _ EmptyBST         _ = Refl
proofGtNInsert x t@(ForkBST l n1 r) n = case owotoNat x n1 of
  EE -> Refl
  LE -> gcastWith (proofGtNLeft t n) (gcastWith (proofGtNInsert x l n) (gcastWith (proofGtNRight t n) Refl))
  GE -> gcastWith (proofGtNRight t n) (gcastWith (proofGtNInsert x r n) (gcastWith (proofGtNLeft t n) Refl))

insert :: Natty n -> BST t -> BST (Insert n t)
insert x EmptyBST         = ForkBST EmptyBST x EmptyBST
insert x (ForkBST l n r)  = case owotoNat x n of
  EE -> ForkBST l n r
  LE -> gcastWith (proofLtNInsert x l n) (ForkBST (insert x l) n r)
  GE -> gcastWith (proofGtNInsert x r n) (ForkBST l n (insert x r))

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
  E  -> n
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

-- | Proofs for delete. Prove that deletion preserves the invariants of BST.

-- | If x ~ Max t, then
-- | LtN (Delete x t) x :~: 'True
proofDelete :: (x ~ Max t) =>
  Natty x -> BST t -> LtN (Delete x t) x :~: 'True
proofDelete _ EmptyBST        = Refl
proofDelete x t@(ForkBST l n r) = case owotoNat x n of
  EE -> case isEmpty l of
    E -> gcastWith (proofLtNTree r n) Refl
    NE -> case isEmpty r of
      E  -> Refl
      NE -> undefined -- | Impossible case since x ~ Max t
  LE -> undefined -- | Impossible case since can't happen x ~ Max t AND owoto x n = LE
  GE -> gcastWith (proofMaxRight x t) (gcastWith (proofDelete x r) (gcastWith (proofLtNTree l x) (gcastWith (proofLtNNode n x) Refl)))

-- | If x ~ Max ('ForkTree l n r), then
-- | x ~ Max r
proofMaxRight :: (t ~ ('ForkTree l n r), Max t ~ x) =>
  Natty x -> BST t -> Max r :~: x
proofMaxRight x (ForkBST _ _ r) = case isEmpty r of
  E  -> undefined -- | Impossible case since Max r must be x
  NE -> gcastWith (proofMaxRight x r) Refl

-- | If x ~ Max l AND t ~ 'ForkTree l n r, then
-- | GtN x r :~: 'True
proofDelete' :: (x ~ Max l, t ~ 'ForkTree l n r) =>
  Natty x -> BST t -> GtN r x :~: 'True
proofDelete' x (ForkBST _ _ r) = gcastWith (proofGtNTree r x) Refl

-- | If Compare x n :~: 'LT AND (ForkTree t n _), then
-- | LtN (Delete x t) n :~: 'True
proofLtNDelete :: (Compare x n ~ 'LT, LtN t n ~ 'True) =>
  Natty x -> BST t -> Natty n -> LtN (Delete x t) n :~: 'True
proofLtNDelete _ EmptyBST         _ = Refl
proofLtNDelete x t@(ForkBST l n1 r) n = case owotoNat x n1 of
  EE -> case isEmpty l of
    E  -> gcastWith (proofLtNRight t n) Refl
    NE -> case isEmpty r of
      E  -> gcastWith (proofLtNLeft t n) Refl
      NE -> undefined -- | Impossible case since Compare x n :~: 'LT
  LE -> gcastWith (proofLtNLeft t n) (gcastWith (proofLtNDelete x l n) (gcastWith (proofLtNRight t n) (gcastWith (proofLtNNode n1 n) Refl)))
  GE -> gcastWith (proofLtNRight t n) (gcastWith (proofLtNDelete x r n) (gcastWith (proofLtNLeft t n) (gcastWith (proofLtNNode n1 n) Refl)))

-- | If Compare x n :~: 'GT AND (ForkTree _ n t), then
-- | GtN n (Delete x t) :~: 'True
proofGtNDelete :: (Compare x n ~ 'GT, GtN t n ~ 'True) =>
  Natty x -> BST t -> Natty n -> GtN (Delete x t) n :~: 'True
proofGtNDelete _ EmptyBST         _ = Refl
proofGtNDelete x t@(ForkBST l n1 r) n = case owotoNat x n1 of
  EE -> case isEmpty l of
    E  -> gcastWith (proofGtNRight t n) Refl
    NE -> case isEmpty r of
      E  -> gcastWith (proofGtNLeft t n) Refl
      NE -> undefined -- | Impossible case since Compare x n :~: 'GT
  LE -> gcastWith (proofGtNLeft t n) (gcastWith (proofGtNDelete x l n) (gcastWith (proofGtNRight t n) (gcastWith (proofGtNNode n1 n) Refl)))
  GE -> gcastWith (proofGtNRight t n) (gcastWith (proofGtNDelete x r n) (gcastWith (proofGtNLeft t n) (gcastWith (proofGtNNode n1 n) Refl)))

delete :: Natty x -> BST t -> BST (Delete x t)
delete _ EmptyBST         = EmptyBST
delete x (ForkBST l n r)  = case owotoNat x n of
  EE -> case isEmpty l of
    E -> r
    NE -> case isEmpty r of
      E -> l
      NE -> gcastWith (proofDelete' maxL (ForkBST l n r)) (gcastWith (proofDelete maxL l) (ForkBST (delete maxL l) maxL r))
        where
          maxL = max l
  LE -> gcastWith (proofLtNDelete x l n) (ForkBST (delete x l) n r)
  GE -> gcastWith (proofGtNDelete x r n) (ForkBST l n (delete x r))
