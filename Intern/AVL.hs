{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE Safe #-}

module Intern.AVL (
  AVL(EmptyAVL,ForkAVL),
  insertAVL,
  lookupAVL,
  deleteAVL
) where

import           Data.Proxy (Proxy)
import           Intern.AVLOperations (AVL(EmptyAVL,ForkAVL), Insertable(Insert,insert), Member, Lookupable(lookup), Deletable(Delete,delete))
import           ITree  (Tree(ForkTree))
import           Node (Node)
import           Prelude (Bool(True))

insertAVL :: (Insertable x a t) =>
  Node x a -> AVL t -> AVL (Insert x a t)
insertAVL = insert

lookupAVL :: (t ~ 'ForkTree l (Node n a1) r, Member x t ~ 'True, Lookupable x a t) =>
  Proxy x -> AVL t -> a
lookupAVL = lookup

deleteAVL :: (Deletable x t) =>
  Proxy x -> AVL t -> AVL (Delete x t)
deleteAVL = delete
