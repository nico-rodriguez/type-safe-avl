{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ITreeNatIncremental.BBT2 where

import Compare (Compare)
import Data.Type.Bool
import Data.Type.Equality
import Data.Nat

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

class IsAA (t :: AATree) where
instance IsAA 'EmptyAATree where
instance (IsAA l, IsAA r, IsAA1 ('ForkAATree l n lv r), IsAA2 ('ForkAATree l n lv r),
  IsAA3 ('ForkAATree l n lv r), IsAA4 ('ForkAATree l n lv r)) =>
  IsAA ('ForkAATree l n lv r)

class IsAA1 (t :: AATree) where
instance IsAA1 'EmptyAATree where
instance IsAA1 ('ForkAATree 'EmptyAATree n lv r) where
instance LtN llv lv =>
  IsAA1 ('ForkAATree ('ForkAATree ll ln llv lr) n lv r) where

class IsAA2 (t :: AATree) where
instance IsAA2 'EmptyAATree where
instance IsAA2 ('ForkAATree l n lv 'EmptyAATree) where
instance LeN rlv lv =>
  IsAA2 ('ForkAATree l n lv ('ForkAATree rl rn rlv rr)) where

class IsAA3 (t :: AATree) where
instance IsAA3 'EmptyAATree where
instance IsAA3 ('ForkAATree 'EmptyAATree n 'Z 'EmptyAATree) where
instance IsAA3 ('ForkAATree 'EmptyAATree n 'Z ('ForkAATree 'EmptyAATree rn 'Z 'EmptyAATree)) where
instance IsAA3 ('ForkAATree l n lv ('ForkAATree rl rn rlv 'EmptyAATree)) where
instance LtN rrlv lv =>
  IsAA3 ('ForkAATree l n lv ('ForkAATree rl rn rlv ('ForkAATree rrl rrn rrlv rrr))) where

class IsAA4 (t :: AATree) where
instance IsAA4 'EmptyAATree where
instance IsAA4 ('ForkAATree l n 'Z r) where
instance IsAA4 ('ForkAATree ('ForkAATree ll ln llv lr) n ('S lv) ('ForkAATree rl rn rlv rr)) where

data ProofIsAA :: AATree -> * where
  PAAE :: ProofIsAA 'EmptyAATree
  PAAF :: (IsAA l, IsAA r, IsAA1 ('ForkAATree l n lv r), IsAA2 ('ForkAATree l n lv r),
    IsAA3 ('ForkAATree l n lv r), IsAA4 ('ForkAATree l n lv r)) =>
    Natty lv -> ProofIsAA ('ForkAATree l n lv r)

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
-- | Impossible cases
proofIsAA (ForkIAATree EmptyIAATree _ Zy (ForkIAATree EmptyIAATree _ _ ForkIAATree{})) = undefined
proofIsAA (ForkIAATree EmptyIAATree _ Zy (ForkIAATree ForkIAATree{} _ _ _)) = undefined
proofIsAA (ForkIAATree EmptyIAATree _ (Sy _) _) = undefined
proofIsAA (ForkIAATree ForkIAATree{} _ Zy _) = undefined
proofIsAA (ForkIAATree ForkIAATree{} _ (Sy _) EmptyIAATree) = undefined
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

data BBT :: AATree -> * where
  BBT :: (IsAA t) => IAATree t -> BBT t

instance Show (BBT t) where
  show (BBT t) = "BBT $ " ++ show t

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
  EE -> let
    t' = ForkIAATree ll ln lv (ForkIAATree lr n lv r)
    in case proofIsAA t' of
      PAAF _ -> t'
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

insertBBT :: Natty x -> BBT t -> BBT (Split (Skew (Insert x t)))
insertBBT x (BBT t) = let
  t' = split $ skew $ insert x t
  in case proofIsAA t' of
    PAAE -> undefined -- | Impossible case since t' has at least x
    PAAF _ -> BBT t'
