{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE Safe               #-}

{-# OPTIONS_GHC -Wno-missing-signatures          #-}
{-# OPTIONS_GHC -Wno-missing-export-lists        #-}
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures    #-}


module Data.Tree.AVL.Unsafe.Examples where

import           Data.Tree.AVL.Unsafe (deleteAVL, emptyAVL, insertAVL,
                                       lookupAVL)
import           Prelude              ()



-- | Test Balanced Binary Tree
avle = emptyAVL
avlt1 = insertAVL 20 'f' avle
avlt2 = insertAVL 60 'o' avlt1
avlt3 = insertAVL 30 'l' avlt2
avlt4 = insertAVL 50 'p' avlt3
avlt5 = insertAVL 0  'f' avlt4
avlt6 = insertAVL 70 'q' avlt5
avlt7 = insertAVL 70 't' avlt6
avlt8 = insertAVL 75 'a' avlt7
avlt9 = insertAVL 80 'a' avlt8
avlt10 = insertAVL 90 'a' avlt9

l1 = lookupAVL 30 avlt8

-- | Nothing: key 10 is not in the tree avlt8
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
