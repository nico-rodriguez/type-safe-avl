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

module Benchmarking.Extern.Operations where

import           Data.Proxy           (Proxy (Proxy))
import           Data.Type.Equality   (type (==))
import           Extern.AVL           (deleteAVL, insertAVL)
import           Extern.AVLOperations (Deletable (Delete), Insertable (Insert))
import           Extern.AVLProofs     (AVL (AVL), ProofIsAVLDelete,
                                       ProofIsAVLInsert, ProofIsBSTDelete,
                                       ProofIsBSTInsert)
import           GHC.TypeLits         (type (-), CmpNat, Nat)
import           ITree                (ITree (EmptyITree, ForkITree),
                                       Tree (EmptyTree, ForkTree))
import           Node                 (Node (), mkNode)
import           Prelude              (Bool (False, True), Char, Ordering (EQ),
                                       undefined, ($))
import           Unsafe.Coerce        (unsafeCoerce)

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
