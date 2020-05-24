{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Tree.AVL.Intern (
  emptyAVL,
  insertAVL,
  lookupAVL,
  deleteAVL
) where

import           Data.Proxy           (Proxy)
import           Data.Tree.AVL.Intern.Operations (AVL (EmptyAVL),
                                       Deletable (Delete, delete),
                                       Insertable (Insert, insert),
                                       Lookupable (lookup), Member)
import           Data.Tree.ITree                (Tree (EmptyTree, ForkTree))
import           Data.Tree.Node                 (Node, mkNode)
import           Prelude              (Bool (True))


emptyAVL :: AVL 'EmptyTree
emptyAVL = EmptyAVL

insertAVL :: (Insertable x a t) =>
  Proxy x -> a -> AVL t -> AVL (Insert x a t)
insertAVL x a = insert node
  where node = mkNode x a

lookupAVL :: (t ~ 'ForkTree l (Node n a1) r, Member x t ~ 'True, Lookupable x a t) =>
  Proxy x -> AVL t -> a
lookupAVL = lookup

deleteAVL :: (Deletable x t) =>
  Proxy x -> AVL t -> AVL (Delete x t)
deleteAVL = delete
