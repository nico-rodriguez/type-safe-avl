{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Tree.BST.FullExtern (
  ProofIsBST(proofIsBST),
  ProofLtN(proofLtN),
  ProofGtN(proofGtN),
  BST (BST),
  ITree (EmptyITree),
  insert, lookup, delete
) where

import           Data.Proxy                       (Proxy (Proxy))
import           Data.Tree.BST.Extern.Constructor (BST (BST))
import           Data.Tree.BST.Extern.Delete      (Deletable (delete))
import           Data.Tree.BST.Extern.Insert      (Insertable (insert))
import           Data.Tree.BST.Extern.Lookup      (Lookupable (lookup))
import           Data.Tree.BST.Invariants         (GtN, IsBST, LtN)
import           Data.Tree.ITree                  (ITree (EmptyITree, ForkITree),
                                                   Tree (EmptyTree, ForkTree))
import           Data.Tree.Node                   (Node)
import           Data.Type.Equality               ((:~:) (Refl), gcastWith)
import           GHC.TypeLits                     (CmpNat, Nat)
import           Prelude                          (Bool (True),
                                                   Ordering (GT, LT), ($))


class ProofLtN (t::Tree) (n::Nat) where
  proofLtN :: ITree t -> Proxy n -> LtN t n :~: 'True
instance ProofLtN 'EmptyTree n where
  proofLtN EmptyITree _ = Refl
instance (ProofLtN l n, ProofLtN r n, CmpNat n1 n ~ 'LT) =>
  ProofLtN ('ForkTree l (Node n1 a) r) n where
  proofLtN (ForkITree l _ r) pn =
    gcastWith (proofLtN r pn) $     -- LtN r n ~ 'True
    gcastWith (proofLtN l pn) Refl  -- LtN l n ~ 'True

class ProofGtN (t::Tree) (n::Nat) where
  proofGtN :: ITree t -> Proxy n -> GtN t n :~: 'True
instance ProofGtN 'EmptyTree n where
  proofGtN EmptyITree _ = Refl
instance (ProofGtN l n, ProofGtN r n, CmpNat n1 n ~ 'GT) =>
  ProofGtN ('ForkTree l (Node n1 a) r) n where
  proofGtN (ForkITree l _ r) pn =
    gcastWith (proofGtN r pn) $     -- GtN r n ~ 'True
    gcastWith (proofGtN l pn) Refl  -- GtN l n ~ 'True

class ProofIsBST (t::Tree) where
  proofIsBST :: ITree t -> IsBST t :~: 'True
instance ProofIsBST 'EmptyTree where
  proofIsBST EmptyITree = Refl
instance (ProofLtN l n, ProofGtN r n, ProofIsBST l, ProofIsBST r) =>
  ProofIsBST ('ForkTree l (Node n a) r) where
  proofIsBST (ForkITree l _ r) =
    gcastWith (proofLtN l (Proxy::Proxy n)) $  -- LtN l n ~ 'True
    gcastWith (proofGtN r (Proxy::Proxy n)) $  -- GtN r n ~ 'True
    gcastWith (proofIsBST r) $                 -- IsBST r ~ 'True
    gcastWith (proofIsBST l) Refl              -- IsBST l ~ 'True
