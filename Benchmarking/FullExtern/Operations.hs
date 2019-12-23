{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Trustworthy           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Benchmarking.FullExtern.Operations where

import           Data.Proxy           (Proxy (Proxy))
import           Data.Type.Equality   ((:~:) (Refl), type (==), gcastWith)
import           Extern.AVLOperations (BalancedHeights,
                                       Deletable (Delete, delete), Height,
                                       Insertable (Insert, insert))
import           Extern.AVLProofs     (AVL (AVL), IsAVL)
import           Extern.BSTProofs     (GtN, IsBST, LtN)
import           GHC.TypeLits         (type (-), CmpNat, Nat)
import           ITree                (ITree (EmptyITree, ForkITree),
                                       Tree (EmptyTree, ForkTree))
import           Node                 (Node)
import           Prelude              (Bool (True), Ordering (EQ, GT, LT), ($))


class ProofLtN (t::Tree) (n::Nat) where
  proofLtN :: ITree t -> Proxy n -> LtN t n :~: 'True
instance ProofLtN 'EmptyTree n where
  proofLtN EmptyITree _ = Refl
instance ( ProofLtN l n, ProofLtN r n, CmpNat n1 n ~ 'LT) =>
  ProofLtN ('ForkTree l (Node n1 a) r) n where
  proofLtN (ForkITree l _ r) pn =
    gcastWith (proofLtN r pn) $
      gcastWith (proofLtN l pn) Refl

class ProofGtN (t::Tree) (n::Nat) where
  proofGtN :: ITree t -> Proxy n -> GtN t n :~: 'True
instance ProofGtN 'EmptyTree n where
  proofGtN EmptyITree _ = Refl
instance ( ProofGtN l n, ProofGtN r n, CmpNat n1 n ~ 'GT) =>
  ProofGtN ('ForkTree l (Node n1 a) r) n where
  proofGtN (ForkITree l _ r) pn =
    gcastWith (proofGtN r pn) $
      gcastWith (proofGtN l pn) Refl

class ProofIsBST (t::Tree) where
  proofIsBST :: ITree t -> IsBST t :~: 'True
instance ProofIsBST 'EmptyTree where
  proofIsBST EmptyITree = Refl
instance (ProofLtN l n, ProofGtN r n, ProofIsBST l, ProofIsBST r) =>
  ProofIsBST ('ForkTree l (Node n a) r) where
  proofIsBST (ForkITree l _ r) =
    gcastWith (proofLtN l (Proxy::Proxy n)) $
      gcastWith (proofGtN r (Proxy::Proxy n)) $
        gcastWith (proofIsBST r) $
          gcastWith (proofIsBST l) Refl

class ProofIsAVL (t::Tree) where
  proofIsAVL :: ITree t -> IsAVL t :~: 'True
instance ProofIsAVL 'EmptyTree where
  proofIsAVL EmptyITree = Refl
instance (BalancedHeights (Height l) (Height r) ~ 'True, ProofLtN l n, ProofGtN r n, ProofIsAVL l, ProofIsAVL r) =>
  ProofIsAVL ('ForkTree l (Node n a) r) where
  proofIsAVL (ForkITree l _ r) =
    gcastWith (proofLtN l (Proxy::Proxy n)) $
      gcastWith (proofGtN r (Proxy::Proxy n)) $
        gcastWith (proofIsAVL r) $
          gcastWith (proofIsAVL l) Refl
