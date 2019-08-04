{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ITreeNatIncremental.BBT where

import Compare (Compare, OWOTO(..))
import Data.Type.Bool
import Data.Type.Equality
import Data.Nat
import ITreeNatIncremental.ITree

class IsBBT (t :: Tree) where
instance IsBBT 'EmptyTree where
instance (IsBBT l, IsBBT r, BalancedHeights (Height l) (Height r)) =>
  IsBBT ('ForkTree l n r) where

type family BalancedHeightsTF (h1 :: Nat) (h2 :: Nat) :: Bool where
  BalancedHeightsTF 'Z      'Z      = 'True
  BalancedHeightsTF ('S 'Z) 'Z      = 'True
  BalancedHeightsTF 'Z      ('S 'Z) = 'True
  BalancedHeightsTF ('S h1) ('S h2) = BalancedHeightsTF h1 h2
  BalancedHeightsTF h1      h2      = 'False

type family RebalanceTree (t :: Tree) :: Tree where
  RebalanceTree 'EmptyTree        = 'EmptyTree
  RebalanceTree ('ForkTree l n ('ForkTree rl rn rr)) =
    (If (BalancedHeightsTF (Height l) (Height ('ForkTree rl rn rr)) == 'True)
      ('ForkTree l n ('ForkTree rl rn rr))  -- | No rebalancing needed
      (If (Compare (Height rl) (Height rr) == 'LT)
        ('ForkTree ('ForkTree l n rl) rn rr)  -- | Simple left rotation
        (RightLeftRotation ('ForkTree l n ('ForkTree rl rn rr)))  -- | Right-left rotation
      )
    )
  RebalanceTree ('ForkTree ('ForkTree ll ln lr) n r) =
    (If (BalancedHeightsTF (Height ('ForkTree ll ln lr)) (Height r) == 'True)
      ('ForkTree ('ForkTree ll ln lr) n r)  -- | No rebalancing needed
      (If (Compare (Height ll) (Height lr) == 'GT)
        ('ForkTree ll ln ('ForkTree lr n r))  -- | Simple right rotation
        (LeftRightRotation ('ForkTree ('ForkTree ll ln lr) n r))  -- | Left-right rotation
      )
    )
  RebalanceTree ('ForkTree l n r) = ('ForkTree l n r)

type family RightLeftRotation (t :: Tree) :: Tree where
  RightLeftRotation ('ForkTree l n ('ForkTree ('ForkTree rll rln rlr) rn rr)) =
    ('ForkTree ('ForkTree l n rll) rln ('ForkTree rlr rn rr))

type family LeftRightRotation (t :: Tree) :: Tree where
  LeftRightRotation ('ForkTree ('ForkTree ll ln ('ForkTree lrl lrn lrr)) n r) =
    ('ForkTree ('ForkTree ll ln lrl) lrn ('ForkTree lrr n r))

data BBT :: Tree -> * where
  BBT :: (IsBBT t) => ITree t -> BBT t

instance Show (BBT t) where
  show (BBT t) = "BBT $ " ++ show t

data ProofBBT :: Tree -> * where
  PBE :: ProofBBT 'EmptyTree
  PBF :: (IsBBT l, IsBBT r, BalancedHeights (Height l) (Height r)) =>
    ProofBBT l -> Natty n -> ProofBBT r -> ProofBBT ('ForkTree l n r)

proofBBT :: ITree t -> ProofBBT t
proofBBT EmptyITree        = PBE
proofBBT (ForkITree l n r) = case proofSubTreeBBT l of
  STBBT _ -> case proofSubTreeBBT r of
    STBBT _ -> case proofHeight l of
      PH _ hl -> case proofHeight r of
        PH _ hr -> case proofBalancedHeights hl hr of
          PBH _ _ -> PBF (proofBBT l) n (proofBBT r)
          NPBH    -> undefined  -- | Unbalanced tree

data SubTreeBBT :: Tree -> * where
  STBBT :: (IsBBT t) =>
    ITree t -> SubTreeBBT t

proofSubTreeBBT :: ITree t -> SubTreeBBT t
proofSubTreeBBT EmptyITree        = STBBT EmptyITree
proofSubTreeBBT (ForkITree l n r) = case proofSubTreeBBT l of
  STBBT _ -> case proofSubTreeBBT r of
    STBBT _ -> case proofHeight l of
      PH _ hl -> case proofHeight r of
        PH _ hr -> case proofBalancedHeights hl hr of
          PBH _ _ -> STBBT (ForkITree l n r)
          NPBH    -> undefined  -- | Unbalanced tree

data ProofHeight :: Tree -> Nat -> *  where
  PH :: (Height t ~ h) => ITree t -> Natty h -> ProofHeight t h

proofHeight :: (Height t ~ h) =>
  ITree t -> ProofHeight t h
proofHeight EmptyITree        = PH EmptyITree Zy
proofHeight t@(ForkITree l _ r) = let
  PH _ hl = proofHeight l
  PH _ hr = proofHeight r
  in case owotoNat hl hr of
    EE -> PH t (Sy hr)
    LE -> PH t (Sy hr)
    GE -> PH t (Sy hl)

class BalancedHeights (h1 :: Nat) (h2 :: Nat) where
instance BalancedHeights 'Z      'Z      where
instance BalancedHeights 'Z      ('S 'Z) where
instance BalancedHeights ('S 'Z) 'Z      where
instance BalancedHeights h1 h2 =>
  BalancedHeights ('S h1) ('S h2)

data ProofBalancedHeights :: Nat -> Nat -> * where
  PBH  :: (BalancedHeights h1 h2) =>
    Natty h1 -> Natty h2 -> ProofBalancedHeights h1 h2
  NPBH :: ProofBalancedHeights h1 h2  -- | For completion only

proofBalancedHeights :: Natty h1 -> Natty h2 -> ProofBalancedHeights h1 h2
proofBalancedHeights Zy      Zy      = PBH Zy Zy
proofBalancedHeights Zy      (Sy Zy) = PBH Zy (Sy Zy)
proofBalancedHeights (Sy Zy) Zy      = PBH (Sy Zy) Zy
proofBalancedHeights (Sy h1) (Sy h2) = case proofBalancedHeights h1 h2 of
  PBH h1' h2' -> PBH (Sy h1') (Sy h2')
  NPBH        -> NPBH
proofBalancedHeights Zy          (Sy (Sy _)) = NPBH
proofBalancedHeights (Sy (Sy _)) Zy          = NPBH

type family Height (t :: Tree) :: Nat where
  Height 'EmptyTree        = 'Z
  Height ('ForkTree l n r) = 'S (MaxNat (Height l) (Height r))

type family MaxNat (n1 :: Nat) (n2 :: Nat) :: Nat where
  MaxNat n1 n2 =
    (If (Compare n1 n2 == 'EQ)
      n1
      (If (Compare n1 n2 == 'LT)
        n2
        n1
      )
    )

insertBBT :: Natty x -> BBT t -> BBT (Insert x t)
insertBBT x (BBT t) = let
  t' = insert x t
  in case proofBBT t' of
    PBE   -> undefined -- | Impossible case since t' = insert x t
    PBF{} -> BBT t'

deleteBBT :: Natty x -> BBT t -> BBT (Delete x t)
deleteBBT x (BBT t) = let
  t' = delete x t
  in case proofBBT t' of
    PBE   -> BBT EmptyITree
    PBF{} -> BBT t'
