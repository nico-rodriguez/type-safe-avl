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

module Benchmarking.Intern.Operations where

import           Data.Proxy           (Proxy (Proxy))
import           Data.Type.Equality   (type (==))
import           GHC.TypeLits         (type (-), CmpNat, Nat)
import           Intern.AVL           (deleteAVL, insertAVL)
import           Intern.AVLOperations (AVL (EmptyAVL, ForkAVL),
                                       Deletable (Delete), Insertable (Insert))
import           ITree                (Tree (EmptyTree, ForkTree))
import           Node                 (Node, mkNode)
import           Prelude              (Bool (False, True), Char, Ordering (EQ),
                                       undefined, ($))
import           Unsafe.Coerce        (unsafeCoerce)

proxyPred :: Proxy n -> Proxy (n - 1)
proxyPred = undefined

type family InsertN (n::Nat) (b::Bool) (t::Tree) :: Tree where
  InsertN 0 'True  t = Insert 0 Char t
  InsertN n 'False t = InsertN (n - 1) (CmpNat (n - 1) 0 == 'EQ) (Insert n Char t)

class InsertNClass (n::Nat) (b::Bool) (t::Tree) where
  insertN :: Proxy n -> Proxy b -> AVL t -> AVL (InsertN n (CmpNat n 0 == 'EQ) t)
instance (Insertable 0 Char t) =>
  InsertNClass 0 'True t where
  insertN _ _  = insertAVL (mkNode (Proxy::Proxy 0) 'a')
instance (InsertNClass (n - 1) (CmpNat (n - 1) 0 == 'EQ) (Insert n Char t), Insertable n Char t) =>
  InsertNClass n 'False t where
  insertN pn _ t = unsafeCoerce $ insertN (proxyPred pn) (Proxy::Proxy (CmpNat (n - 1) 0 == 'EQ)) (insertAVL (mkNode pn 'a') t)


type family DeleteN (n::Nat) (t::Tree) where
  DeleteN n 'EmptyTree                  = 'EmptyTree
  DeleteN n ('ForkTree l (Node n1 a) r) = DeleteN (n - 1) (Delete n ('ForkTree l (Node n1 a) r))

class DeleteNClass (n::Nat) (t::Tree) where
  deleteN :: Proxy n -> AVL t -> AVL (DeleteN n t)
instance DeleteNClass n 'EmptyTree where
  deleteN _  EmptyAVL     = EmptyAVL
instance (DeleteNClass (n - 1) (Delete n ('ForkTree l (Node n1 a) r)), Deletable n ('ForkTree l (Node n1 a) r)) =>
  DeleteNClass n ('ForkTree l (Node n1 a) r) where
  deleteN pn t@ForkAVL{}  = deleteN (proxyPred pn) (deleteAVL pn t)
