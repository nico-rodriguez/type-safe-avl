{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Tree.AVL.Extern.Examples where

import Data.Proxy (Proxy (Proxy))
import Data.Tree.AVL.Extern
  ( deleteAVL,
    emptyAVL,
    insertAVL,
    lookupAVL,
  )
import Prelude (Bool (False, True), Float, Int, String)

-- | Proxies for the node keys
p0 = Proxy :: Proxy 0

p1 = Proxy :: Proxy 1

p2 = Proxy :: Proxy 2

p3 = Proxy :: Proxy 3

p4 = Proxy :: Proxy 4

p5 = Proxy :: Proxy 5

p6 = Proxy :: Proxy 6

p7 = Proxy :: Proxy 7

-- | Test Balanced Binary Tree
avle = emptyAVL

avlt1 = insertAVL (Proxy :: Proxy 20) 'f' avle

avlt2 = insertAVL (Proxy :: Proxy 60) (4 :: Int) avlt1

avlt3 = insertAVL (Proxy :: Proxy 30) "lala" avlt2

avlt4 = insertAVL (Proxy :: Proxy 50) True avlt3

avlt5 = insertAVL (Proxy :: Proxy 0) ([1, 2, 3] :: [Int]) avlt4

avlt6 = insertAVL (Proxy :: Proxy 70) (1.8 :: Float) avlt5

avlt7 = insertAVL (Proxy :: Proxy 70) [False] avlt6

avlt8 = insertAVL (Proxy :: Proxy 75) 'a' avlt7

avlt9 = insertAVL (Proxy :: Proxy 80) 'a' avlt8

avlt10 = insertAVL (Proxy :: Proxy 90) 'a' avlt9

l1' :: String
l1' = lookupAVL (Proxy :: Proxy 30) avlt8

-- | Error: key 10 is not in the tree avlt8
-- err = lookupAVL (Proxy::Proxy 10) avlt8
avlt11 = deleteAVL (Proxy :: Proxy 20) avlt8

avlt12 = deleteAVL (Proxy :: Proxy 60) avlt8

avlt13 = deleteAVL (Proxy :: Proxy 30) avlt8

avlt14 = deleteAVL (Proxy :: Proxy 50) avlt8

avlt15 = deleteAVL (Proxy :: Proxy 0) avlt8

avlt16 = deleteAVL (Proxy :: Proxy 70) avlt8

avlt17 = deleteAVL (Proxy :: Proxy 70) avlt8

avlt18 = deleteAVL (Proxy :: Proxy 75) avlt8

avlt19 = deleteAVL (Proxy :: Proxy 21) avlt8

avlt20 = deleteAVL (Proxy :: Proxy 21) avle
