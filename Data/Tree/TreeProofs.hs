{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

module Data.Tree.TreeProofs (
    proofGtNGTGtN    
) where

import           Data.Proxy      (Proxy)
import           Data.Type.Equality ((:~:)(Refl))
import           Data.Tree.ITree (Tree(ForkTree))
import           Data.Tree.BST.Invariants (LtN, GtN)
import           GHC.TypeNats    (type (+), type (-), type (<=?), Nat, CmpNat)
import           Prelude         (Bool (False, True), Ordering(LT,GT))
import           Unsafe.Coerce   (unsafeCoerce)


-- | If all keys in a tree are greater than a given Nat n which is greater than another Nat n',
-- | then all keys of the tree are greater than n'
-- | forall (t::Tree) (n,n'::Nat), GtN t n && CmpNat n n' ~ 'GT -> GtN t n'
proofGtNGTGtN :: Proxy t -> Proxy n -> Proxy n' -> GtN t n :~: 'True -> CmpNat n n' :~: 'GT -> GtN t n' :~: 'True
proofGtNGTGtN _ _ _ _ _ = unsafeCoerce Refl
