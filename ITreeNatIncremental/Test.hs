{-# LANGUAGE DataKinds #-}

module ITreeNatIncremental.Test where

import           Data.Nat
import           Data.Proxy
import           ITreeNatIncremental.BST
import           ITreeNatIncremental.Node
import           ITreeNatIncremental.ITree

p0 :: Proxy 'Z
p0 = Proxy

p1 :: Proxy ('S 'Z)
p1 = Proxy

p2 :: Proxy ('S ('S 'Z))
p2 = Proxy

p3 :: Proxy ('S ('S ('S 'Z)))
p3 = Proxy

p4 :: Proxy ('S ('S ('S ('S 'Z))))
p4 = Proxy

p5 :: Proxy ('S ('S ('S ('S ('S 'Z)))))
p5 = Proxy

p6 :: Proxy ('S ('S ('S ('S ('S ('S 'Z))))))
p6 = Proxy

p7 :: Proxy ('S ('S ('S ('S ('S ('S ('S 'Z)))))))
p7 = Proxy

e = BST EmptyITree

t1 = insertBST (mkNode p4 'f') e
t2 = insertBST (mkNode p2 (4::Int)) t1
t3 = insertBST (mkNode p6 "lala") t2
t4 = insertBST (mkNode p3 True) t3
t5 = insertBST (mkNode p5 ([1,2,3]::[Int])) t4
t6 = insertBST (mkNode p0 (1.8::Float)) t5
t7 = insertBST (mkNode p7 [False]) t6

data SomeData = SD
  deriving (Show)

t8 = insertBST (mkNode p7 SD) t7

l1 :: [Char]
l1 = lookupBST p6 t8

-- | Error: key p1 ('S 'Z) is not in the tree t8
-- err = lookupBST p1 t8
--
-- t9 = deleteBST sbn7 t7
-- t10 = deleteBST sbn4 t7
-- t11 = deleteBST sbn1 t7
--
-- | Test Balanced Binary Tree
-- be = BBST EmptyIAATree
--
-- bt = insertBBST sbn7 $ insertBBST sbn6 $ insertBBST sbn5 $ insertBBST sbn4 $ insertBBST sbn3 $ insertBBST sbn2 $ insertBBST sbn1 $ insertBBST sbn0 be
-- bt1 = insertBBST sbn4 be
-- bt2 = insertBBST sbn2 bt1
-- bt3 = insertBBST sbn6 bt2
-- bt4 = insertBBST sbn3 bt3
-- bt5 = insertBBST sbn5 bt4
-- bt6 = insertBBST sbn0 bt5
-- bt7 = insertBBST sbn7 bt6
-- bt8 = insertBBST sbn7 bt7
--
-- bt9 = deleteBBST sbn7 bt7
-- bt10 = deleteBBST sbn4 bt7
-- bt11 = deleteBBST sbn1 bt7
