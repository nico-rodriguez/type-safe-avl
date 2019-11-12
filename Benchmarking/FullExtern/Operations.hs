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
import           Node                 (Node, mkNode)
import           Prelude              (Bool (False, True), Char,
                                       Ordering (EQ, GT, LT), undefined, ($))
import           Unsafe.Coerce        (unsafeCoerce)

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

type family InsertN (n::Nat) (b::Bool) (t::Tree) :: Tree where
  InsertN 0 'True  t = Insert 0 Char t
  InsertN n 'False t = InsertN (n - 1) (CmpNat (n - 1) 0 == 'EQ) (Insert n Char t)

class InsertNClass (n::Nat) (b::Bool) where
  insertN :: Proxy n -> Proxy b -> AVL (InsertN n b 'EmptyTree)
instance (b ~ (CmpNat n 0 == 'EQ), ProofIsBST (InsertN n b 'EmptyTree), ProofIsAVL (InsertN n b 'EmptyTree), InsertNClass' n b 'EmptyTree) =>
  InsertNClass n b where
  insertN pn pb = gcastWith (proofIsAVL t) $ gcastWith (proofIsBST t) $ AVL t
                    where
                      t = insertN' pn pb EmptyITree

class InsertNClass' (n::Nat) (b::Bool) (t::Tree) where
  insertN' :: Proxy n -> Proxy b -> ITree t -> ITree (InsertN n (CmpNat n 0 == 'EQ) t)
instance (Insertable 0 Char t) =>
  InsertNClass' 0 'True t where
  insertN' _ _  = insert (mkNode (Proxy::Proxy 0) 'a')
instance (InsertNClass' (n - 1) (CmpNat (n - 1) 0 == 'EQ) (Insert n Char t), Insertable n Char t) =>
  InsertNClass' n 'False t where
  insertN' pn _ t = unsafeCoerce $ insertN' (proxyPred pn) (Proxy::Proxy (CmpNat (n - 1) 0 == 'EQ)) (insert (mkNode pn 'a') t)


type family DeleteN (n::Nat) (t::Tree) where
  DeleteN n 'EmptyTree                  = 'EmptyTree
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
instance DeleteNClass' n 'EmptyTree where
  deleteN' _  EmptyITree    = EmptyITree
instance (DeleteNClass' (n - 1) (Delete n ('ForkTree l (Node n1 a) r)), Deletable n ('ForkTree l (Node n1 a) r)) =>
  DeleteNClass' n ('ForkTree l (Node n1 a) r) where
  deleteN' pn t@ForkITree{} = deleteN' (proxyPred pn) (delete pn t)
