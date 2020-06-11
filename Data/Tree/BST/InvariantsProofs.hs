{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

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
import           Data.Type.Equality ((:~:)(Refl))
import           GHC.TypeNats       (CmpNat)
import           Prelude            (Bool (True), Ordering (GT, LT))
import           Unsafe.Coerce      (unsafeCoerce)


-- | In a BST, the keys on the left sub tree are lesser than the key of the root
-- | forall (l,r::Tree) (n::Nat) (a::Type), IsBST ('ForkTree l (Node n a) r) -> LtN l n
proofIsBSTLtN ::
    Proxy ('ForkTree l (Node n a) r) -> IsBST ('ForkTree l (Node n a) r) :~: 'True -> LtN l n :~: 'True
proofIsBSTLtN _ _ =  unsafeCoerce Refl

-- | In a BST, the keys on the right sub tree are bigger than the key of the root
-- | forall (l,r::Tree) (n::Nat) (a::Type), IsBST ('ForkTree l (Node n a) r) -> GtN r n
proofIsBSTGtN ::
    Proxy ('ForkTree l (Node n a) r) -> IsBST ('ForkTree l (Node n a) r) :~: 'True -> GtN r n :~: 'True
proofIsBSTGtN _ _ = unsafeCoerce Refl


-- | In a BST, the left sub tree is also BST
-- | forall (l,r::Tree) (n::Nat) (a::Type), IsBST ('ForkTree l (Node n a) r) -> IsBST l
proofIsBSTLeftSubTree ::
    Proxy ('ForkTree l (Node n a) r) -> IsBST ('ForkTree l (Node n a) r) :~: 'True -> IsBST l :~: 'True
proofIsBSTLeftSubTree _ _ = unsafeCoerce Refl

-- | In a BST, the right sub tree is also BST
-- | forall (l,r::Tree) (n::Nat) (a::Type), IsBST ('ForkTree l (Node n a) r) -> IsBST r
proofIsBSTRightSubTree ::
    Proxy ('ForkTree l (Node n a) r) -> IsBST ('ForkTree l (Node n a) r) :~: 'True -> IsBST r :~: 'True
proofIsBSTRightSubTree _ _ = unsafeCoerce Refl


-- | If all keys of a tree are lesser than a given Nat,
-- | the same goes for it's left sub tree
-- | forall (l,r::Tree) (n,n'::Nat) (a::Type), LtN ('ForkTree l (Node n a) r) n' -> LtN l n'
proofLtNLeftSubTree ::
    Proxy ('ForkTree l (Node n a) r) -> Proxy n' -> LtN ('ForkTree l (Node n a) r) n' :~: 'True -> LtN l n' :~: 'True
proofLtNLeftSubTree _ _ = unsafeCoerce Refl

-- | If all keys of a tree are lesser than a given Nat,
-- | the key of the root is lesser than that given Nat
-- | forall (l,r::Tree) (n,n'::Nat) (a::Type), LtN ('ForkTree l (Node n a) r) n' -> CmpNat n n' ~ 'LT
proofLtNLT ::
    Proxy ('ForkTree l (Node n a) r) -> Proxy n' -> LtN ('ForkTree l (Node n a) r) n' :~: 'True -> CmpNat n n' :~: 'LT
proofLtNLT _ _ _ = unsafeCoerce Refl

-- | If all keys of a tree are lesser than a given Nat,
-- | the same goes for it's right sub tree
-- | forall (l,r::Tree) (n,n'::Nat) (a::Type), LtN ('ForkTree l (Node n a) r) n' -> LtN r n'
proofLtNRightSubTree ::
    Proxy ('ForkTree l (Node n a) r) -> Proxy n' -> LtN ('ForkTree l (Node n a) r) n' :~: 'True -> LtN r n' :~: 'True
proofLtNRightSubTree _ _ _ = unsafeCoerce Refl


-- | If all keys of a tree are bigger than a given Nat,
-- | the same goes for it's left sub tree
-- | forall (l,r::Tree) (n,n'::Nat) (a::Type), GtN ('ForkTree l (Node n a) r) n' -> GtN l n'
proofGtNLeftSubTree ::
    Proxy ('ForkTree l (Node n a) r) -> Proxy n' -> GtN ('ForkTree l (Node n a) r) n' :~: 'True -> GtN l n' :~: 'True
proofGtNLeftSubTree _ _ _ = unsafeCoerce Refl

-- | If all keys of a tree are bigger than a given Nat,
-- | the key of the root is bigger than that given Nat
-- | forall (l,r::Tree) (n,n'::Nat) (a::Type), GtN ('ForkTree l (Node n a) r) n' -> CmpNat n n' ~ 'GT
proofGtNGT ::
    Proxy ('ForkTree l (Node n a) r) -> Proxy n' -> GtN ('ForkTree l (Node n a) r) n' :~: 'True -> CmpNat n n' :~: 'GT
proofGtNGT _ _ _ = unsafeCoerce Refl

-- | If all keys of a tree are bigger than a given Nat,
-- | the same goes for it's right sub tree
-- | forall (l,r::Tree) (n,n'::Nat) (a::Type), GtN ('ForkTree l (Node n a) r) n' -> GtN r n'
proofGtNRightSubTree ::
    Proxy ('ForkTree l (Node n a) r) -> Proxy n' -> GtN ('ForkTree l (Node n a) r) n' :~: 'True -> GtN r n' :~: 'True
proofGtNRightSubTree _ _ _ = unsafeCoerce Refl
