{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Tree.AVL.FullExtern (
  ProofIsBST(proofIsBST),
  ProofIsAVL(proofIsAVL),
  AVL (AVL),
  ITree (EmptyITree),
  insert, lookup, delete
) where

import           Data.Proxy                       (Proxy (Proxy))
import           Data.Tree.AVL.Extern.Constructor (AVL (AVL))
import           Data.Tree.AVL.Extern.Delete      (Deletable (delete))
import           Data.Tree.AVL.Extern.Insert      (Insertable (insert))
import           Data.Tree.AVL.Invariants         (BalancedHeights, Height,
                                                   IsAVL)
import           Data.Tree.BST.Extern.Lookup      (Lookupable (lookup))
import           Data.Tree.BST.FullExtern         (ProofGtN (proofGtN),
                                                   ProofIsBST (proofIsBST),
                                                   ProofLtN (proofLtN))
import           Data.Tree.ITree                  (ITree (EmptyITree, ForkITree),
                                                   Tree (EmptyTree, ForkTree))
import           Data.Tree.Node                   (Node)
import           Data.Type.Equality               ((:~:) (Refl), gcastWith)
import           Prelude                          (Bool (True), ($))


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
