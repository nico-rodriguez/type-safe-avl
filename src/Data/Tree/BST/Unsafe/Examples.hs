{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE Safe               #-}

{-# OPTIONS_GHC -Wno-missing-signatures          #-}
{-# OPTIONS_GHC -Wno-missing-export-lists        #-}
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures    #-}


module Data.Tree.BST.Unsafe.Examples where

import           Data.Tree.BST.Unsafe (deleteBST, emptyBST, insertBST,
                                       lookupBST)
import           Prelude              ()



-- | Test Balanced Binary Tree
bste = emptyBST
bstt1 = insertBST 20 'f' bste
bstt2 = insertBST 60 'a' bstt1
bstt3 = insertBST 30 'l' bstt2
bstt4 = insertBST 50 'g' bstt3
bstt5 = insertBST 0  'o' bstt4
bstt6 = insertBST 70 'p' bstt5
bstt7 = insertBST 70 'u' bstt6
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
