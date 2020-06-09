{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Tree.BST.InvariantsProofs (
  proofIsBSTLtN, proofIsBSTGtN,
  proofIsBSTLeftSubTree, proofIsBSTRightSubTree,
  proofLtNLeftSubTree, proofLtNLT, proofLtNRightSubTree,
  proofGtNLeftSubTree, proofGtNGT, proofGtNRightSubTree
) where

import           Data.Proxy         (Proxy)
import           Data.Tree.ITree    (Tree (ForkTree))
import           Data.Tree.Node     (Node)
import           Data.Tree.BST.Invariants (LtN, GtN, IsBST)
import           Data.Type.Equality ((:~:)(Refl),)
import           GHC.TypeNats       (CmpNat)
import           Prelude            (Bool (True), Ordering (GT, LT))
import           Unsafe.Coerce      (unsafeCoerce)


-- | IsBST ('ForkTree l (Node n a) r) -> LtN l n
proofIsBSTLtN :: (IsBST ('ForkTree l (Node n a) r) ~ 'True) =>
    Proxy ('ForkTree l (Node n a) r) -> LtN l n :~: 'True
proofIsBSTLtN _ =  unsafeCoerce Refl

-- | IsBST ('ForkTree l (Node n a) r) -> GtN l n
proofIsBSTGtN :: (IsBST ('ForkTree l (Node n a) r) ~ 'True) =>
    Proxy ('ForkTree l (Node n a) r) -> GtN r n :~: 'True
proofIsBSTGtN _ = unsafeCoerce Refl


-- | IsBST ('ForkTree l (Node n a) r) -> IsBST l
proofIsBSTLeftSubTree :: (IsBST ('ForkTree l (Node n a) r) ~ 'True) =>
    Proxy ('ForkTree l (Node n a) r) -> IsBST l :~: 'True
proofIsBSTLeftSubTree _ = unsafeCoerce Refl

-- | IsBST ('ForkTree l (Node n a) r) -> IsBST r
proofIsBSTRightSubTree :: (IsBST ('ForkTree l (Node n a) r) ~ 'True) =>
    Proxy ('ForkTree l (Node n a) r) -> IsBST r :~: 'True
proofIsBSTRightSubTree _ = unsafeCoerce Refl


-- | LtN ('ForkTree l (Node n a) r) n' -> LtN l n'
proofLtNLeftSubTree :: (LtN ('ForkTree l (Node n a) r) n' ~ 'True) =>
    Proxy ('ForkTree l (Node n a) r) -> Proxy n' -> LtN l n' :~: 'True
proofLtNLeftSubTree _ = unsafeCoerce Refl

-- | LtN ('ForkTree l (Node n a) r) n' -> CmpNat n n' ~ 'LT
proofLtNLT :: (LtN ('ForkTree l (Node n a) r) n' ~ 'True) =>
    Proxy ('ForkTree l (Node n a) r) -> Proxy n' -> CmpNat n n' :~: 'LT
proofLtNLT _ _ = unsafeCoerce Refl

-- | LtN ('ForkTree l (Node n a) r) n' -> LtN r n'
proofLtNRightSubTree :: (LtN ('ForkTree l (Node n a) r) n' ~ 'True) =>
    Proxy ('ForkTree l (Node n a) r) -> Proxy n' -> LtN r n' :~: 'True
proofLtNRightSubTree _ _ = unsafeCoerce Refl


-- | GtN ('ForkTree l (Node n a) r) n' -> GtN l n'
proofGtNLeftSubTree :: (GtN ('ForkTree l (Node n a) r) n' ~ 'True) =>
    Proxy ('ForkTree l (Node n a) r) -> Proxy n' -> GtN l n' :~: 'True
proofGtNLeftSubTree _ _ = unsafeCoerce Refl

-- | GtN ('ForkTree l (Node n a) r) n' -> CmpNat n n' ~ 'GT
proofGtNGT :: (GtN ('ForkTree l (Node n a) r) n' ~ 'True) =>
    Proxy ('ForkTree l (Node n a) r) -> Proxy n' -> CmpNat n n' :~: 'GT
proofGtNGT _ _ = unsafeCoerce Refl

-- | GtN ('ForkTree l (Node n a) r) n' -> GtN r n'
proofGtNRightSubTree :: (GtN ('ForkTree l (Node n a) r) n' ~ 'True) =>
    Proxy ('ForkTree l (Node n a) r) -> Proxy n' -> GtN r n' :~: 'True
proofGtNRightSubTree _ _ = unsafeCoerce Refl
