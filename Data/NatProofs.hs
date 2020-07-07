{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

module Data.NatProofs (
    proofLTGT, proofGTLT
) where

import           Data.Proxy      (Proxy)
import           Data.Type.Equality ((:~:)(Refl))
import           GHC.TypeNats    (CmpNat)
import           Prelude         (Ordering(LT,GT))
import           Unsafe.Coerce   (unsafeCoerce)


-- | If n is lesser than m, then m is greater than n
-- | forall (n,m::Nat), CmpNat n m ~ 'LT -> CmpNat m n ~ 'GT
proofLTGT :: Proxy n -> Proxy m -> CmpNat n m :~: 'LT -> CmpNat m n :~: 'GT
proofLTGT _ _ _ = unsafeCoerce Refl

-- | If n is greater than m, then m is lesser than n
-- | forall (n,m::Nat), CmpNat n m ~ 'GT -> CmpNat m n ~ 'LT
proofGTLT :: Proxy n -> Proxy m -> CmpNat n m :~: 'GT -> CmpNat m n :~: 'LT
proofGTLT _ _ _ = unsafeCoerce Refl
