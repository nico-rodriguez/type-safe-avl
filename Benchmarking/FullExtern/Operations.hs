{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Benchmarking.Extern.Operations where

import           Data.Proxy
import           Data.Type.Equality
import           Node
import           ITree
import           Extern.AVL
import           Extern.AVLOperations
import           Extern.AVLProofs
import           GHC.TypeLits
import           Unsafe.Coerce

proxyPred :: Proxy n -> Proxy (n - 1)
proxyPred = undefined

type family InsertN (n::Nat) :: Tree where
  InsertN 0 = 'EmptyTree
  InsertN n = Insert n Char (InsertN (n - 1))

class InsertNClass (n::Nat) (b::Bool) where
  insertN :: Proxy n -> Proxy b -> AVL (InsertN n)
instance InsertNClass 0 'True where
  insertN _ _ = AVL EmptyITree
instance (InsertNClass (n - 1) (CmpNat (n - 1) 0 == 'EQ), Insertable n Char (InsertN (n - 1)), ProofIsAVLInsert n Char (InsertN (n - 1)), ProofIsBSTInsert n Char (InsertN (n - 1))) =>
  InsertNClass n 'False where
  insertN pn _ = unsafeCoerce $ insertAVL (mkNode pn 'a') (insertN (proxyPred pn) (Proxy::Proxy (CmpNat (n - 1) 0 == 'EQ)))

t1 = insertN (Proxy::Proxy 0) (Proxy::Proxy 'True)
t2 = insertN (Proxy::Proxy 1) (Proxy::Proxy 'False)
t3 = insertN (Proxy::Proxy 9) (Proxy::Proxy 'False)
t4 = insertAVL (mkNode (Proxy::Proxy 10) 'a') t3


type family DeleteN (n::Nat) (t::Tree) where
  DeleteN 0 'EmptyTree                  = 'EmptyTree
  DeleteN n ('ForkTree l (Node n1 a) r) = DeleteN (n - 1) (Delete n ('ForkTree l (Node n1 a) r))

class DeleteNClass (n::Nat) (t::Tree) where
  deleteN :: Proxy n -> AVL t -> AVL (DeleteN n t)
instance DeleteNClass 0 'EmptyTree where
  deleteN _  (AVL EmptyITree)    = AVL EmptyITree
instance (DeleteNClass (n - 1) (Delete n ('ForkTree l (Node n1 a) r)), Deletable n ('ForkTree l (Node n1 a) r),
  ProofIsAVLDelete n ('ForkTree l (Node n1 a) r), ProofIsBSTDelete n ('ForkTree l (Node n1 a) r)) =>
  DeleteNClass n ('ForkTree l (Node n1 a) r) where
  deleteN pn t@(AVL ForkITree{}) = deleteN (proxyPred pn) (deleteAVL pn t)

e = deleteN (Proxy::Proxy 10) t4
-- t5' = deleteAVL (Proxy::Proxy 10) t4
-- t5 = deleteAVL (Proxy::Proxy 9) t5'
-- t6 = deleteAVL (Proxy::Proxy 8) t5
-- t7 = deleteAVL (Proxy::Proxy 7) t6
-- t8 = deleteAVL (Proxy::Proxy 6) t7
-- t9 = deleteAVL (Proxy::Proxy 5) t8
-- t10 = deleteAVL (Proxy::Proxy 4) t9
-- t11 = deleteAVL (Proxy::Proxy 3) t10
-- t12 = deleteAVL (Proxy::Proxy 2) t11
-- t13 = deleteAVL (Proxy::Proxy 1) t12
-- t14 = deleteAVL (Proxy::Proxy 0) t13
-- t15 = deleteAVL (Proxy::Proxy ) t14
