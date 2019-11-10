{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Trustworthy #-}

module Benchmarking.Extern.Operations where

import           Prelude (Char, Bool(True,False), Ordering(EQ), undefined, ($))
import           Data.Proxy (Proxy(Proxy))
import           Data.Type.Equality (type (==))
import           Node (Node(), mkNode)
import           ITree (Tree(EmptyTree,ForkTree), ITree(EmptyITree,ForkITree))
import           Extern.AVL (insertAVL, deleteAVL)
import           Extern.AVLOperations (Insertable(Insert), Deletable(Delete))
import           Extern.AVLProofs (AVL(AVL), ProofIsAVLInsert, ProofIsBSTInsert, ProofIsAVLDelete, ProofIsBSTDelete)
import           GHC.TypeLits (Nat, type (-), CmpNat)
import           Unsafe.Coerce (unsafeCoerce)

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
