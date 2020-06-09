{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Tree.AVL.InvariantsProofs (
    proofBalancedHeightsSym,
    proofAVLBalancedHeights,
    proofIsAVLLeft, proofIsAVLRight
) where

import           Data.Proxy      (Proxy)
import           Data.Tree.ITree (Tree (EmptyTree, ForkTree))
import           Data.Tree.Node  (Node)
import           Data.Tree.AVL.Invariants (Height, BalancedHeights, IsAVL)
import           Data.Type.Bool  (type (&&), If)
import           Data.Type.Equality ((:~:)(Refl))
import           GHC.TypeNats    (type (+), type (-), type (<=?), Nat)
import           Prelude         (Bool (False, True), undefined)
import           Unsafe.Coerce   (unsafeCoerce)


-- | Symmetry of BalancedHeights
-- | forall (lh,rh::Nat), BalancedHeights lh rh ~ BalancedHeights rh lh
proofBalancedHeightsSym :: Proxy lh -> Proxy rh -> BalancedHeights lh rh :~: BalancedHeights rh lh
proofBalancedHeightsSym _ _ = unsafeCoerce Refl


-- | The left sub tree of an AVL is also an AVL
-- | forall (l,r::Tree) (n::Nat) (a::Type), IsAVL ('ForkTree l (Node n a) r) -> IsAVL l
proofIsAVLLeft :: (IsAVL ('ForkTree l (Node n a) r) ~ 'True) =>
    Proxy ('ForkTree l (Node n a) r) -> IsAVL l :~: 'True
proofIsAVLLeft _ = unsafeCoerce Refl

-- | The right sub tree of an AVL is also an AVL
-- | forall (l,r::Tree) (n::Nat) (a::Type), IsAVL ('ForkTree l (Node n a) r) -> IsAVL r
proofIsAVLRight :: (IsAVL ('ForkTree l (Node n a) r) ~ 'True) =>
    Proxy ('ForkTree l (Node n a) r) -> IsAVL r :~: 'True
proofIsAVLRight _ = unsafeCoerce Refl


-- | The heights of the left and right sub trees of an AVL are balanced
-- | forall (l,r::Tree) (n::Nat) (a::Type),
-- |   IsAVL ('ForkTree l (Node n a) r) -> BalancedHeights (Height l) (Height r)
proofAVLBalancedHeights :: (IsAVL ('ForkTree l (Node n a) r) ~ 'True) =>
    Proxy ('ForkTree l (Node n a) r) -> BalancedHeights (Height l) (Height r) :~: 'True
proofAVLBalancedHeights _ = unsafeCoerce Refl
