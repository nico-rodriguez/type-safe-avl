{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}

{-# LANGUAGE Safe               #-}

{-# OPTIONS_GHC -Wno-missing-signatures          #-}
{-# OPTIONS_GHC -Wno-missing-export-lists        #-}
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}


module Data.Tree.BST.Unsafe.Examples where

import           Data.Tree.BST.Unsafe (deleteBST, emptyBST, insertBST,
                                       lookupBST)
import           Prelude              (Int, Float, Bool(True,False))



-- | Test Balanced Binary Tree
bste = emptyBST
bstt1 = insertBST 20 'f' bste
bstt2 = insertBST 60 (4::Int) bstt1
bstt3 = insertBST 30 "lala" bstt2
bstt4 = insertBST 50 True bstt3
bstt5 = insertBST 0 ([1,2,3]::[Int]) bstt4
bstt6 = insertBST 70 (1.8::Float) bstt5
bstt7 = insertBST 70 [False] bstt6
bstt8 = insertBST 75 'a' bstt7
bstt9 = insertBST 80 'a' bstt8
bstt10 = insertBST 90 'a' bstt9

l1 = lookupBST 30 bstt8

-- | Nothing: key 10 is not in the tree bstt8
n = lookupBST 10 bstt8

bstt11 = deleteBST 20 bstt8
bstt12 = deleteBST 60 bstt8
bstt13 = deleteBST 30 bstt8
bstt14 = deleteBST 50 bstt8
bstt15 = deleteBST 0 bstt8
bstt16 = deleteBST 70 bstt8
bstt17 = deleteBST 70 bstt8
bstt18 = deleteBST 75 bstt8
bstt19 = deleteBST 21 bstt8
bstt20 = deleteBST 21 bste
