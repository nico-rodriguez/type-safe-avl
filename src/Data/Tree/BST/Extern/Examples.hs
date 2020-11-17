{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}

{-# LANGUAGE Safe               #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}


module Data.Tree.BST.Extern.Examples where

import           Data.Proxy           (Proxy (Proxy))
import           Data.Tree.BST.Extern (deleteBST, emptyBST, insertBST,
                                       lookupBST)
import           Prelude              (Bool (False, True), Float, Int, Show,
                                       String)

p0 = Proxy :: Proxy 0
p1 = Proxy :: Proxy 1
p2 = Proxy :: Proxy 2
p3 = Proxy :: Proxy 3
p4 = Proxy :: Proxy 4
p5 = Proxy :: Proxy 5
p6 = Proxy :: Proxy 6
p7 = Proxy :: Proxy 7

bste = emptyBST

bst1 = insertBST p4 'f' bste
bst2 = insertBST p2 (4::Int) bst1
bst3 = insertBST p6 "lala" bst2
bst4 = insertBST p3 True bst3
bst5 = insertBST p5 ([1,2,3]::[Int]) bst4
bst6 = insertBST p0 (1.8::Float) bst5
bst7 = insertBST p7 [False] bst6

data SomeData = SD
  deriving stock Show

bst8 = insertBST p7 SD bst7

l1 :: String
l1 = lookupBST p6 bst8

-- | Error: key p1 ('S 'Z) is not in the tree bst8
-- err = lookupBST p1 bst8

bst9 = deleteBST p7 bst7
bst10 = deleteBST p4 bst7
bst11 = deleteBST p1 bst7
bst12 = deleteBST p0 bst7
bst13 = deleteBST p2 bst7
bst14 = deleteBST p6 bst7
bst15 = deleteBST p5 bst7
bst16 = deleteBST p3 bst7
