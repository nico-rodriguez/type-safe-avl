{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Tree.BST.FullExtern (
  ProofIsBST(proofIsBST),
  BST (BST),
  ITree (EmptyITree),
  insert, lookup, delete
) where

import           Data.Proxy           (Proxy (Proxy))
import           Data.Type.Equality   ((:~:) (Refl), gcastWith)
import           Data.Tree.BST.Extern.Operations (Insertable(insert),
                                      Lookupable(lookup),
                                      Deletable(delete))
import           Data.Tree.BST.Extern.Proofs     (GtN, IsBST, LtN, BST (BST))
import           GHC.TypeLits         (CmpNat, Nat)
import           Data.Tree.ITree                (ITree (EmptyITree, ForkITree),
                                       Tree (EmptyTree, ForkTree))
import           Data.Tree.Node                 (Node)
import           Prelude              (Bool (True), Ordering (GT, LT), ($))


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
