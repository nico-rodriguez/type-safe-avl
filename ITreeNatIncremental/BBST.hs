{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Balanced binary search trees. The balance condition is from AA Trees

module ITreeNatIncremental.BBST where

import           Data.Nat
import           Data.Type.Bool
import           Data.Type.Equality

data AATree :: * where
  EmptyAATree :: AATree
  ForkAATree  :: AATree -- | left sub tree
    -> Nat              -- | key
    -> Nat              -- | level
    -> AATree           -- | right sub tree
    -> AATree

data IAATree :: AATree -> * where
  EmptyIAATree :: IAATree 'EmptyAATree
  ForkIAATree  :: IAATree l -> Natty n -> Natty lv -> IAATree r -> IAATree ('ForkAATree l n lv r)

instance Show (IAATree t) where
  show EmptyIAATree         = "E"
  show (ForkIAATree l n lv r)  = "F " ++ go l ++ " " ++ show n ++ " " ++ show lv ++ " " ++ go r
    where
      go :: IAATree t' -> String
      go EmptyIAATree         = "E"
      go (ForkIAATree l' n' lv' r')  = "(F " ++ go l' ++ " " ++ show n' ++ " " ++ show lv' ++ " " ++ go r' ++ ")"

-- | All elements of the tree are strictly less than the Nat
class TLtN (t :: AATree) (x :: Nat) where
instance TLtN 'EmptyAATree x where
instance (TLtN l x, Compare n x ~ 'LT, TLtN r x) =>
  TLtN ('ForkAATree l n lv r) x

-- | All elements of the tree are strictly greater than the Nat
class TGtN (t :: AATree) (x :: Nat) where
instance TGtN 'EmptyAATree x where
instance (TGtN l x, Compare n x ~ 'GT, TGtN r x) =>
  TGtN ('ForkAATree l n lv r) x

-- | Class of Binary Search Trees
class IsBST (t :: AATree) where
instance IsBST 'EmptyAATree where
instance (IsBST l, IsBST r, TLtN l n, TGtN r n) =>
  IsBST ('ForkAATree l n lv r) where

-- | Proof object for Binary Search Trees
data ProofBST :: AATree -> * where
  PE :: ProofBST 'EmptyAATree
  PF :: (IsBST l, IsBST r, TLtN l n, TGtN r n, IsBST ('ForkAATree l n lv r)) =>
    ProofBST l -> Natty n -> Natty lv -> ProofBST r -> ProofBST ('ForkAATree l n lv r)

-- | Construct the proof object that the given tree is in fact a Binary Search Tree
proofBST :: IAATree t -> ProofBST t
proofBST EmptyIAATree        = PE
proofBST (ForkIAATree l n lv r) = case proofGTN r n of
  PGTN _ _ -> case proofLTN l n of
    PLTN _ _ -> PF (proofBST l) n lv (proofBST r)

-- | Proof object that all elements of a tree are strictly less than a given Nat
data LTN :: AATree -> Nat -> * where
  PLTN :: (TLtN t n, IsBST t) =>
    IAATree t -> Natty n -> LTN t n

-- | Construct the proof object that all elements of the given tree
-- | are strictly less than a given Nat
proofLTN :: IAATree t -> Natty n -> LTN t n
proofLTN EmptyIAATree         n = PLTN EmptyIAATree n
proofLTN t@(ForkIAATree l n1 _ r) n = case proofLTN l n of
  PLTN _ _ -> case proofLTN r n of
    PLTN _ _ -> case owotoNat n1 n of
      EE -> undefined -- | Impossible case since we want to prove LtN t n
      LE -> case proofBST t of
        PF{} -> PLTN t n
      GE -> undefined -- | Impossible case since we want to prove LtN t n

-- | Proof object that all elements of a tree are strictly greater than a given Nat
data GTN :: AATree -> Nat -> * where
  PGTN :: (TGtN t n, IsBST t) =>
    IAATree t -> Natty n -> GTN t n

-- | Construct the proof object that all elements of the given tree
-- | are strictly greater than a given Nat
proofGTN :: IAATree t -> Natty n -> GTN t n
proofGTN EmptyIAATree           n = PGTN EmptyIAATree n
proofGTN t@(ForkIAATree l n1 _ r) n = case proofGTN r n of
  PGTN _ _ -> case proofGTN l n of
    PGTN _ _ -> case owotoNat n1 n of
      EE -> undefined -- | Impossible case since we want to prove GtN t n
      LE -> undefined -- | Impossible case since we want to prove GtN t n
      GE -> case proofBST t of
        PF{} -> PGTN t n

-- | Class of AA Trees
class IsAA (t :: AATree) where
instance IsAA 'EmptyAATree where
instance (IsAA l, IsAA r, IsAA1 ('ForkAATree l n lv r), IsAA2 ('ForkAATree l n lv r),
  IsAA3 ('ForkAATree l n lv r), IsAA4 ('ForkAATree l n lv r)) =>
  IsAA ('ForkAATree l n lv r)

-- | Constraint: the level of the left child (if it exists) is strictly less
-- | than the level of the root
class IsAA1 (t :: AATree) where
instance IsAA1 'EmptyAATree where
instance IsAA1 ('ForkAATree 'EmptyAATree n lv r) where
instance LtN llv lv =>
  IsAA1 ('ForkAATree ('ForkAATree ll ln llv lr) n lv r) where

-- | Constraint: the level of the right child (if it exists) is equal to or
-- | less than the level of the root
class IsAA2 (t :: AATree) where
instance IsAA2 'EmptyAATree where
instance IsAA2 ('ForkAATree l n lv 'EmptyAATree) where
instance LeN rlv lv =>
  IsAA2 ('ForkAATree l n lv ('ForkAATree rl rn rlv rr)) where

-- | Constraint: the level of the right child of the right child is strictly
-- | less than the level of the root
class IsAA3 (t :: AATree) where
instance IsAA3 'EmptyAATree where
instance IsAA3 ('ForkAATree 'EmptyAATree n 'Z 'EmptyAATree) where
instance IsAA3 ('ForkAATree 'EmptyAATree n 'Z ('ForkAATree 'EmptyAATree rn 'Z 'EmptyAATree)) where
instance IsAA3 ('ForkAATree l n lv ('ForkAATree rl rn rlv 'EmptyAATree)) where
instance LtN rrlv lv =>
  IsAA3 ('ForkAATree l n lv ('ForkAATree rl rn rlv ('ForkAATree rrl rrn rrlv rrr))) where

-- | Constraint: all intermediate nodes (nodes of level greater than zero) have two children
class IsAA4 (t :: AATree) where
instance IsAA4 'EmptyAATree where
instance IsAA4 ('ForkAATree l n 'Z r) where
instance IsAA4 ('ForkAATree ('ForkAATree ll ln llv lr) n ('S lv) ('ForkAATree rl rn rlv rr)) where

-- | Proof object that shows the invariants of AA Trees
data ProofIsAA :: AATree -> * where
  PAAE :: ProofIsAA 'EmptyAATree
  PAAF :: (IsAA l, IsAA r, IsAA1 ('ForkAATree l n lv r), IsAA2 ('ForkAATree l n lv r),
    IsAA3 ('ForkAATree l n lv r), IsAA4 ('ForkAATree l n lv r)) =>
    Natty lv -> ProofIsAA ('ForkAATree l n lv r)

-- | Construct the proof object that the given tree is in fact an AA Tree
proofIsAA :: IAATree t -> ProofIsAA t
proofIsAA EmptyIAATree = PAAE
proofIsAA (ForkIAATree EmptyIAATree _ Zy EmptyIAATree) = PAAF Zy
proofIsAA (ForkIAATree l@(ForkIAATree _ _ llv _) _ (Sy lv) r@(ForkIAATree _ _ rlv EmptyIAATree)) = case owotoNat llv (Sy lv) of
  EE -> undefined -- | Impossible case because it violates IsAA1
  LE -> case owotoNat rlv (Sy lv) of
    EE -> case proofIsAA l of
      PAAF _ -> case proofIsAA r of
        PAAF _ -> PAAF (Sy lv)
    LE -> case proofIsAA l of
      PAAF _ -> case proofIsAA r of
        PAAF _ -> PAAF (Sy lv)
    GE -> undefined -- | Impossible case because it violates IsAA2
  GE -> undefined -- | Impossible case because it violates IsAA1
proofIsAA (ForkIAATree EmptyIAATree _ Zy r@(ForkIAATree EmptyIAATree _ rlv EmptyIAATree)) =
  case owotoNat Zy rlv of
    EE -> case proofIsAA r of
      PAAF _ -> PAAF Zy
    LE -> undefined -- | Impossible case since right level must be equal to Zy
    GE -> undefined -- | Impossible case since right level must be equal to Zy
proofIsAA (ForkIAATree l@(ForkIAATree _ _ llv _) _ (Sy lv) r@(ForkIAATree _ _ rlv (ForkIAATree _ _ rrlv _))) =
  case owotoNat llv (Sy lv) of
    EE -> undefined -- | Impossible case because it violates IsAA1
    LE -> case owotoNat rrlv (Sy lv) of
      EE -> undefined -- | Impossible case because it violates IsAA3
      LE -> case owotoNat rlv (Sy lv) of
        EE -> case proofIsAA l of
          PAAF _ -> case proofIsAA r of
            PAAF _ -> PAAF (Sy lv)
        LE -> case proofIsAA l of
          PAAF _ -> case proofIsAA r of
            PAAF _ -> PAAF (Sy lv)
        GE -> undefined -- | Impossible case because it violates IsAA2
      GE -> undefined -- | Impossible case because it violates IsAA3
    GE -> undefined -- | Impossible case because it violates IsAA1
-- | Impossible cases
proofIsAA (ForkIAATree EmptyIAATree _ Zy (ForkIAATree EmptyIAATree _ _ ForkIAATree{})) = undefined
proofIsAA (ForkIAATree EmptyIAATree _ Zy (ForkIAATree ForkIAATree{} _ _ _)) = undefined
proofIsAA (ForkIAATree EmptyIAATree _ (Sy _) _) = undefined
proofIsAA (ForkIAATree ForkIAATree{} _ Zy _) = undefined
proofIsAA (ForkIAATree ForkIAATree{} _ (Sy _) EmptyIAATree) = undefined

data BBST :: AATree -> * where
  BBST :: (IsBST t, IsAA t) => IAATree t -> BBST t

instance Show (BBST t) where
  show (BBST t) = "BBST $ " ++ show t

type family Insert (x :: Nat) (t :: AATree) :: AATree where
  Insert x 'EmptyAATree = 'ForkAATree 'EmptyAATree x 'Z 'EmptyAATree
  Insert x ('ForkAATree l n lv r) =
    (If (Compare x n == 'EQ)
      ('ForkAATree l n lv r)
      (If (Compare x n == 'LT)
        (Split (Skew ('ForkAATree (Insert x l) n lv r)))
        (Split (Skew ('ForkAATree l n lv (Insert x r))))
      )
    )

type family Skew (t :: AATree) :: AATree where
  Skew ('ForkAATree ('ForkAATree ll ln llv lr) n lv r) =
    (If (Compare llv lv == 'EQ)
      ('ForkAATree ll ln lv ('ForkAATree lr n lv r))
      ('ForkAATree ('ForkAATree ll ln llv lr) n lv r)
    )
  Skew t = t

skew :: IAATree t -> IAATree (Skew t)
skew EmptyIAATree = EmptyIAATree
skew t@(ForkIAATree EmptyIAATree _ _ _) = t
skew t@(ForkIAATree (ForkIAATree ll ln llv lr) n lv r) = case owotoNat llv lv of
  EE -> ForkIAATree ll ln lv (ForkIAATree lr n lv r)
  LE -> t
  GE -> t

type family Split (t :: AATree) :: AATree where
  Split ('ForkAATree l n lv ('ForkAATree rl rn rlv ('ForkAATree rrl rrn rrlv rrr))) =
    (If (Compare lv rlv == 'EQ && Compare rlv rrlv == 'EQ)
      ('ForkAATree ('ForkAATree l n lv rl) rn ('S lv) ('ForkAATree rrl rrn lv rrr))
      ('ForkAATree l n lv ('ForkAATree rl rn rlv ('ForkAATree rrl rrn rrlv rrr)))
    )
  Split t = t

split :: IAATree t -> IAATree (Split t)
split EmptyIAATree = EmptyIAATree
split t@(ForkIAATree _ _ _ EmptyIAATree) = t
split t@(ForkIAATree _ _ _ (ForkIAATree _ _ _ EmptyIAATree)) = t
split t@(ForkIAATree l n lv (ForkIAATree rl rn rlv (ForkIAATree rrl rrn rrlv rrr))) =
  case owotoNat lv rlv of
    EE -> case owotoNat rlv rrlv of
      EE -> ForkIAATree (ForkIAATree l n lv rl) rn (Sy lv) (ForkIAATree rrl rrn lv rrr)
      LE -> t
      GE -> t
    LE -> t
    GE -> t

insert :: Natty x -> IAATree t -> IAATree (Insert x t)
insert x EmptyIAATree         = ForkIAATree EmptyIAATree x Zy EmptyIAATree
insert x (ForkIAATree l n lv r)  = case owotoNat x n of
  EE -> ForkIAATree l n lv r
  LE -> split $ skew $ ForkIAATree (insert x l) n lv r
  GE -> split $ skew $ ForkIAATree l n lv (insert x r)

insertBBST :: Natty x -> BBST t -> BBST (Insert x t)
insertBBST x (BBST t) = let
  t' = insert x t
  in case proofIsAA t' of
    PAAE -> undefined -- | Impossible case since t' has at least x
    PAAF _ -> case proofBST t' of
      PF{} -> BBST t'

member :: Natty x -> IAATree t -> Bool
member _ EmptyIAATree           = False
member x (ForkIAATree l n _ r) = case owotoNat x n of
  EE -> True
  LE -> member x l
  GE -> member x r

memberBBST :: Natty x -> BBST t -> Bool
memberBBST x (BBST t) = member x t

type family Adjust (t :: AATree) :: AATree where
  -- | (1) Right child has two levels lover than root
  -- | (1.a) left child is a simple node
  Adjust ('ForkAATree 'EmptyAATree n ('S lv) ('ForkAATree rl rn rlv rr)) =
    (If (Compare rlv lv == 'LT)
      (Skew ('ForkAATree 'EmptyAATree n lv ('ForkAATree rl rn rlv rr)))
      ('ForkAATree 'EmptyAATree n ('S lv) ('ForkAATree rl rn rlv rr))
    )
  Adjust ('ForkAATree ('ForkAATree ll ln llv 'EmptyAATree) n ('S lv) ('ForkAATree rl rn rlv rr)) =
    (If (Compare rlv lv == 'LT)
      (Skew ('ForkAATree ('ForkAATree ll ln llv 'EmptyAATree) n lv ('ForkAATree rl rn rlv rr)))
      ('ForkAATree ('ForkAATree ll ln llv 'EmptyAATree) n ('S lv) ('ForkAATree rl rn rlv rr))
    )
  -- | TODO
  Adjust ('ForkAATree ('ForkAATree ll ln llv ('ForkAATree lrl lrn lrlv lrr)) n ('S lv) ('ForkAATree rl rn rlv rr)) =
    (If (Compare lrlv llv == 'LT)
      (If (Compare rlv lv == 'LT)
        (Skew ('ForkAATree ('ForkAATree ll ln llv 'EmptyAATree) n lv ('ForkAATree rl rn rlv rr)))
        ('ForkAATree ('ForkAATree ll ln llv 'EmptyAATree) n ('S lv) ('ForkAATree rl rn rlv rr))
      )
      -- | (1.a) left child is a double node
      'EmptyAATree
    )

adjust :: IAATree t -> IAATree (Adjust t)
adjust t@(ForkIAATree EmptyIAATree n (Sy lv) (ForkIAATree rl rn rlv rr)) =
  case owotoNat rlv lv of
    EE -> t
    LE -> ForkIAATree EmptyIAATree n lv (ForkIAATree rl rn rlv rr)
    GE -> t
adjust t@(ForkIAATree (ForkIAATree ll ln llv EmptyIAATree) n (Sy lv) (ForkIAATree rl rn rlv rr)) =
  case owotoNat rlv lv of
    EE -> t
    LE -> skew $ ForkIAATree (ForkIAATree ll ln llv EmptyIAATree) n lv (ForkIAATree rl rn rlv rr)
    GE -> t
