{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Trustworthy #-}

module Benchmarking.FullExtern.Operations where

import           Data.Proxy (Proxy(Proxy))
import           Data.Type.Equality (type (==), gcastWith, (:~:)(Refl))
import           Node (Node, mkNode)
import           ITree (Tree(EmptyTree,ForkTree), ITree(EmptyITree,ForkITree))
import           Extern.AVLOperations (Insertable(Insert,insert), Deletable(Delete,delete), BalancedHeights, Height)
import           Extern.AVLProofs (AVL(AVL), IsAVL)
import           Extern.BSTProofs (IsBST, LtN, GtN)
import           GHC.TypeLits (Nat, type (-), CmpNat)
import           Unsafe.Coerce (unsafeCoerce)

proxyPred :: Proxy n -> Proxy (n - 1)
proxyPred = undefined

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

type family InsertN (n::Nat) :: Tree where
  InsertN 0 = 'EmptyTree
  InsertN n = Insert n Char (InsertN (n - 1))

class InsertNClass (n::Nat) (b::Bool) where
  insertN :: Proxy n -> Proxy b -> AVL (InsertN n)
instance (ProofIsBST (InsertN n), ProofIsAVL (InsertN n), InsertNClass' n b) =>
  InsertNClass n b where
  insertN pn pb = gcastWith (proofIsAVL t) $ gcastWith (proofIsBST t) $ AVL t
                    where
                      t = insertN' pn pb

class InsertNClass' (n::Nat) (b::Bool) where
  insertN' :: Proxy n -> Proxy b -> ITree (InsertN n)
instance InsertNClass' 0 'True where
  insertN' _ _ = EmptyITree
instance (InsertNClass' (n - 1) (CmpNat (n - 1) 0 == 'EQ), Insertable n Char (InsertN (n - 1))) =>
  InsertNClass' n 'False where
  insertN' pn _ = unsafeCoerce $ insert (mkNode pn 'a') (insertN' (proxyPred pn) (Proxy::Proxy (CmpNat (n - 1) 0 == 'EQ)))


type family DeleteN (n::Nat) (t::Tree) where
  DeleteN 0 'EmptyTree                  = 'EmptyTree
  DeleteN n ('ForkTree l (Node n1 a) r) = DeleteN (n - 1) (Delete n ('ForkTree l (Node n1 a) r))

class DeleteNClass (n::Nat) (t::Tree) where
  deleteN :: Proxy n -> AVL t -> AVL (DeleteN n t)
instance (ProofIsAVL (DeleteN (n - 1) (Delete n ('ForkTree l (Node n1 a) r))), ProofIsBST (DeleteN (n - 1) (Delete n ('ForkTree l (Node n1 a) r))),
  Deletable n ('ForkTree l (Node n1 a) r), DeleteNClass' (n - 1) (Delete n ('ForkTree l (Node n1 a) r))) =>
  DeleteNClass n ('ForkTree l (Node n1 a) r) where
  deleteN pn (AVL t@ForkITree{}) = gcastWith (proofIsAVL t') $ gcastWith (proofIsBST t') $ AVL t'
                                    where
                                      t' = deleteN' pn t

class DeleteNClass' (n::Nat) (t::Tree) where
  deleteN' :: Proxy n -> ITree t -> ITree (DeleteN n t)
instance DeleteNClass' 0 'EmptyTree where
  deleteN' _  EmptyITree    = EmptyITree
instance (DeleteNClass' (n - 1) (Delete n ('ForkTree l (Node n1 a) r)), Deletable n ('ForkTree l (Node n1 a) r)) =>
  DeleteNClass' n ('ForkTree l (Node n1 a) r) where
  deleteN' pn t@ForkITree{} = deleteN' (proxyPred pn) (delete pn t)
