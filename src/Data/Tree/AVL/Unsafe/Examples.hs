{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}

{-# LANGUAGE Safe               #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}


module Data.Tree.AVL.Unsafe.Examples where

import           Data.Proxy           (Proxy (Proxy))
import           Data.Tree.AVL.Unsafe (deleteAVL, emptyAVL, insertAVL,
                                       lookupAVL)
import           Prelude              (Int, Float, Maybe, Bool(True,False))

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
avlt1 = insertAVL 20 'f' avle
avlt2 = insertAVL 60 (4::Int) avlt1
avlt3 = insertAVL 30 "lala" avlt2
avlt4 = insertAVL 50 True avlt3
avlt5 = insertAVL 0 ([1,2,3]::[Int]) avlt4
avlt6 = insertAVL 70 (1.8::Float) avlt5
avlt7 = insertAVL 70 [False] avlt6
avlt8 = insertAVL 75 'a' avlt7
avlt9 = insertAVL 80 'a' avlt8
avlt10 = insertAVL 90 'a' avlt9

l1 :: Maybe a
l1 = lookupAVL 30 avlt8

-- | Nothing: key 10 is not in the tree avlt8
n :: Maybe a
n = lookupAVL 10 avlt8

avlt11 = deleteAVL 20 avlt8
avlt12 = deleteAVL 60 avlt8
avlt13 = deleteAVL 30 avlt8
avlt14 = deleteAVL 50 avlt8
avlt15 = deleteAVL 0 avlt8
avlt16 = deleteAVL 70 avlt8
avlt17 = deleteAVL 70 avlt8
avlt18 = deleteAVL 75 avlt8
avlt19 = deleteAVL 21 avlt8
avlt20 = deleteAVL 21 avle
