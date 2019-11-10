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

type family InsertN (n::Nat) :: Tree where
  InsertN 0 = 'EmptyTree
  InsertN n = Insert n Char (InsertN (n - 1))

class InsertNClass (n::Nat) (b::Bool) where
  insertN :: Proxy n -> Proxy b -> AVL (InsertN n)
instance InsertNClass 0 'True where
  insertN _ _ = EmptyAVL
instance (InsertNClass (n - 1) (CmpNat (n - 1) 0 == 'EQ), Insertable n Char (InsertN (n - 1))) =>
  InsertNClass n 'False where
  insertN pn _ = unsafeCoerce $ insertAVL (mkNode pn 'a') (insertN (proxyPred pn) (Proxy::Proxy (CmpNat (n - 1) 0 == 'EQ)))


type family DeleteN (n::Nat) (t::Tree) where
  DeleteN 0 'EmptyTree                  = 'EmptyTree
  DeleteN n ('ForkTree l (Node n1 a) r) = DeleteN (n - 1) (Delete n ('ForkTree l (Node n1 a) r))

class DeleteNClass (n::Nat) (t::Tree) where
  deleteN :: Proxy n -> AVL t -> AVL (DeleteN n t)
instance DeleteNClass 0 'EmptyTree where
  deleteN _  EmptyAVL     = EmptyAVL
instance (DeleteNClass (n - 1) (Delete n ('ForkTree l (Node n1 a) r)), Deletable n ('ForkTree l (Node n1 a) r)) =>
  DeleteNClass n ('ForkTree l (Node n1 a) r) where
  deleteN pn t@ForkAVL{}  = deleteN (proxyPred pn) (deleteAVL pn t)
