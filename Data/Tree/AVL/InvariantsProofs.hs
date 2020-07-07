{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Data.Tree.AVL.InvariantsProofs (
    proofBalancedHeightsSym,
    proofAVLBalancedHeights,
    proofIsAVLLeftSubTree, proofIsAVLRightSubTree
) where

import           Data.Proxy               (Proxy)
import           Data.Tree.AVL.Invariants (BalancedHeights, Height, IsAVL)
import           Data.Tree.ITree          (Tree (ForkTree))
import           Data.Tree.Node           (Node)
import           Data.Type.Equality       ((:~:) (Refl))
import           Prelude                  (Bool (True))
import           Unsafe.Coerce            (unsafeCoerce)


-- | Symmetry of BalancedHeights
-- | forall (lh,rh::Nat), BalancedHeights lh rh ~ BalancedHeights rh lh
proofBalancedHeightsSym :: Proxy lh -> Proxy rh -> BalancedHeights lh rh :~: BalancedHeights rh lh
proofBalancedHeightsSym _ _ = unsafeCoerce Refl


-- | The left sub tree of an AVL is also an AVL
-- | forall (l,r::Tree) (n::Nat) (a::Type), IsAVL ('ForkTree l (Node n a) r) -> IsAVL l
proofIsAVLLeftSubTree ::
    Proxy ('ForkTree l (Node n a) r) -> IsAVL ('ForkTree l (Node n a) r) :~: 'True -> IsAVL l :~: 'True
proofIsAVLLeftSubTree _ _ = unsafeCoerce Refl

-- | The right sub tree of an AVL is also an AVL
-- | forall (l,r::Tree) (n::Nat) (a::Type), IsAVL ('ForkTree l (Node n a) r) -> IsAVL r
proofIsAVLRightSubTree ::
    Proxy ('ForkTree l (Node n a) r) -> IsAVL ('ForkTree l (Node n a) r) :~: 'True -> IsAVL r :~: 'True
proofIsAVLRightSubTree _ _ = unsafeCoerce Refl


-- | The heights of the left and right sub trees of an AVL are balanced
-- | forall (l,r::Tree) (n::Nat) (a::Type),
-- |   IsAVL ('ForkTree l (Node n a) r) -> BalancedHeights (Height l) (Height r)
proofAVLBalancedHeights :: (IsAVL ('ForkTree l (Node n a) r) ~ 'True) =>
    Proxy ('ForkTree l (Node n a) r) -> BalancedHeights (Height l) (Height r) :~: 'True
proofAVLBalancedHeights _ = unsafeCoerce Refl
