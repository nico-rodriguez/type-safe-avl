{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe                  #-}

module Intern.AVL (
  AVL(EmptyAVL,ForkAVL),
  emptyAVL,
  insertAVL,
  lookupAVL,
  deleteAVL
) where

import           Data.Proxy           (Proxy)
import           Intern.AVLOperations (AVL (EmptyAVL, ForkAVL),
                                       Deletable (Delete, delete),
                                       Insertable (Insert, insert),
                                       Lookupable (lookup), Member)
import           ITree                (Tree (EmptyTree, ForkTree))
import           Node                 (Node)
import           Prelude              (Bool (True))


emptyAVL :: AVL 'EmptyTree
emptyAVL = EmptyAVL

insertAVL :: (Insertable x a t) =>
  Node x a -> AVL t -> AVL (Insert x a t)
insertAVL = insert

lookupAVL :: (t ~ 'ForkTree l (Node n a1) r, Member x t ~ 'True, Lookupable x a t) =>
  Proxy x -> AVL t -> a
lookupAVL = lookup

deleteAVL :: (Deletable x t) =>
  Proxy x -> AVL t -> AVL (Delete x t)
deleteAVL = delete
