{-# LANGUAGE DataKinds #-}

module ITreeNatIncremental.Test where

import           Data.Proxy
import           ITreeNatIncremental.AVL
import           ITreeNatIncremental.BST
import           ITreeNatIncremental.ITree
import           ITreeNatIncremental.Node

p0 :: Proxy 0
p0 = Proxy
p1 :: Proxy 1
p1 = Proxy
p2 :: Proxy 2
p2 = Proxy
p3 :: Proxy 3
p3 = Proxy
p4 :: Proxy 4
p4 = Proxy
p5 :: Proxy 5
p5 = Proxy
p6 :: Proxy 6
p6 = Proxy
p7 :: Proxy 7
p7 = Proxy

-- e :: BST 'EmptyTree
-- e = BST EmptyITree
--
-- t1 :: BST (Insert 4 Char 'EmptyTree)
-- t1 = insertBST (mkNode p4 'f') e
-- t2 :: BST (Insert 2 Int ('ForkTree 'EmptyTree (Node 4 Char) 'EmptyTree))
-- t2 = insertBST (mkNode p2 (4::Int)) t1
-- t3 :: BST (Insert 6 String ('ForkTree ('ForkTree 'EmptyTree (Node 2 Int) 'EmptyTree) (Node 4 Char) 'EmptyTree))
-- t3 = insertBST (mkNode p6 "lala") t2
-- t4 :: BST (Insert 3 Bool ('ForkTree ('ForkTree 'EmptyTree (Node 2 Int) 'EmptyTree) (Node 4 Char) ('ForkTree 'EmptyTree (Node 6 String) 'EmptyTree)))
-- t4 = insertBST (mkNode p3 True) t3
-- t5 :: BST (Insert 5 [Int] ('ForkTree ('ForkTree 'EmptyTree (Node 2 Int) ('ForkTree 'EmptyTree (Node 3 Bool) 'EmptyTree)) (Node 4 Char) ('ForkTree 'EmptyTree (Node 6 String) 'EmptyTree)))
-- t5 = insertBST (mkNode p5 ([1,2,3]::[Int])) t4
-- t6 :: BST (Insert 0 Float ('ForkTree ('ForkTree 'EmptyTree (Node 2 Int) ('ForkTree 'EmptyTree (Node 3 Bool) 'EmptyTree)) (Node 4 Char) ('ForkTree ('ForkTree 'EmptyTree (Node 5 [Int]) 'EmptyTree) (Node 6 String) 'EmptyTree)))
-- t6 = insertBST (mkNode p0 (1.8::Float)) t5
-- t7 :: BST (Insert 7 [Bool] ('ForkTree ('ForkTree ('ForkTree 'EmptyTree (Node 0 Float) 'EmptyTree) (Node 2 Int) ('ForkTree 'EmptyTree (Node 3 Bool) 'EmptyTree)) (Node 4 Char) ('ForkTree ('ForkTree 'EmptyTree (Node 5 [Int]) 'EmptyTree) (Node 6 String) 'EmptyTree)))
-- t7 = insertBST (mkNode p7 [False]) t6
--
-- data SomeData = SD
--   deriving (Show)
--
-- t8 :: BST (Insert 7 SomeData ('ForkTree ('ForkTree ('ForkTree 'EmptyTree (Node 0 Float) 'EmptyTree) (Node 2 Int) ('ForkTree 'EmptyTree (Node 3 Bool) 'EmptyTree)) (Node 4 Char) ('ForkTree ('ForkTree 'EmptyTree (Node 5 [Int]) 'EmptyTree) (Node 6 String) ('ForkTree 'EmptyTree (Node 7 [Bool]) 'EmptyTree))))
-- t8 = insertBST (mkNode p7 SD) t7
--
-- l1 :: String
-- l1 = lookupBST p6 t8
--
-- -- | Error: key p1 ('S 'Z) is not in the tree t8
-- -- err = lookupBST p1 t8
--
-- t9' = let
--   BST t7' = t7
--   in delete p7 t7'
-- t10' = let
--   BST t7' = t7
--   in delete p4 t7'
-- t11' = let
--   BST t7' = t7
--   in delete p1 t7'
-- t12' = let
--   BST t7' = t7
--   in delete p0 t7'
-- t13' = let
--   BST t7' = t7
--   in delete p2 t7'
-- t14' = let
--   BST t7' = t7
--   in delete p6 t7'
-- t15' = let
--   BST t7' = t7
--   in delete p5 t7'
--
-- t9 = deleteBST p7 t7
-- t10 = deleteBST p4 t7
-- t11 = deleteBST p1 t7
-- t12 = deleteBST p0 t7
-- t13 = deleteBST p2 t7
-- t14 = deleteBST p6 t7
-- t15 = deleteBST p5 t7
-- t16 = deleteBST p3 t7

-- | Test Balanced Binary Tree
be = AVL EmptyITree
t1 = insertAVL (mkNode p0 'f') be
t2 = insertAVL (mkNode p1 (4::Int)) t1
t3 = insertAVL (mkNode p3 "lala") t2
t4 = insertAVL (mkNode p4 True) t3
t5 = insertAVL (mkNode p5 ([1,2,3]::[Int])) t4
t6 = insertAVL (mkNode p2 (1.8::Float)) t5
t7 = insertAVL (mkNode p7 [False]) t6
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
