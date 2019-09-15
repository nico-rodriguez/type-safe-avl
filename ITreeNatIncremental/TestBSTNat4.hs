{-# LANGUAGE DataKinds #-}

module ITreeNatIncremental.Test4 where

import           Data.Nat
import           Data.Proxy
import           ITreeNatIncremental.BSTNat4

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

e :: BST 'EmptyTree
e = BST EmptyITree

t1 :: BST (Insert ('S ('S ('S ('S 'Z)))) Char 'EmptyTree)
t1 = insertBST (mkNode p4 'f') e

t2 :: BST (Insert ('S ('S 'Z)) Int ('ForkTree 'EmptyTree (Node ('S ('S ('S ('S 'Z)))) Char) 'EmptyTree))
t2 = insertBST (mkNode p2 (4::Int)) t1

t3 :: BST (Insert ('S ('S ('S ('S ('S ('S 'Z)))))) [Char] ('ForkTree ('ForkTree 'EmptyTree (Node ('S ('S 'Z)) Int) 'EmptyTree) (Node ('S ('S ('S ('S 'Z)))) Char) 'EmptyTree))
t3 = insertBST (mkNode p6 "lala") t2

t4 :: BST (Insert ('S ('S ('S 'Z))) Bool ('ForkTree ('ForkTree 'EmptyTree (Node ('S ('S 'Z)) Int) 'EmptyTree) (Node ('S ('S ('S ('S 'Z)))) Char) ('ForkTree 'EmptyTree (Node ('S ('S ('S ('S ('S ('S 'Z)))))) [Char]) 'EmptyTree)))
t4 = insertBST (mkNode p3 True) t3

t5 :: BST (Insert ('S ('S ('S ('S ('S 'Z))))) [Int] ('ForkTree ('ForkTree 'EmptyTree (Node ('S ('S 'Z)) Int) ('ForkTree 'EmptyTree (Node ('S ('S ('S 'Z))) Bool) 'EmptyTree)) (Node ('S ('S ('S ('S 'Z)))) Char) ('ForkTree 'EmptyTree (Node ('S ('S ('S ('S ('S ('S 'Z)))))) [Char]) 'EmptyTree)))
t5 = insertBST (mkNode p5 ([1,2,3]::[Int])) t4

t6 :: BST (Insert 'Z Float ('ForkTree ('ForkTree 'EmptyTree (Node ('S ('S 'Z)) Int) ('ForkTree 'EmptyTree (Node ('S ('S ('S 'Z))) Bool) 'EmptyTree)) (Node ('S ('S ('S ('S 'Z)))) Char) ('ForkTree ('ForkTree 'EmptyTree (Node ('S ('S ('S ('S ('S 'Z))))) [Int]) 'EmptyTree) (Node ('S ('S ('S ('S ('S ('S 'Z)))))) [Char]) 'EmptyTree)))
t6 = insertBST (mkNode p0 (1.8::Float)) t5

t7 :: BST (Insert ('S ('S ('S ('S ('S ('S ('S 'Z))))))) [Bool] ('ForkTree ('ForkTree ('ForkTree 'EmptyTree (Node 'Z Float) 'EmptyTree) (Node ('S ('S 'Z)) Int) ('ForkTree 'EmptyTree (Node ('S ('S ('S 'Z))) Bool) 'EmptyTree)) (Node ('S ('S ('S ('S 'Z)))) Char) ('ForkTree ('ForkTree 'EmptyTree (Node ('S ('S ('S ('S ('S 'Z))))) [Int]) 'EmptyTree) (Node ('S ('S ('S ('S ('S ('S 'Z)))))) [Char]) 'EmptyTree)))
t7 = insertBST (mkNode p7 [False]) t6

data SomeData = SD
  deriving (Show)

t8 :: BST (Insert ('S ('S ('S ('S ('S ('S ('S 'Z))))))) SomeData ('ForkTree ('ForkTree ('ForkTree 'EmptyTree (Node 'Z Float) 'EmptyTree) (Node ('S ('S 'Z)) Int) ('ForkTree 'EmptyTree (Node ('S ('S ('S 'Z))) Bool) 'EmptyTree)) (Node ('S ('S ('S ('S 'Z)))) Char) ('ForkTree ('ForkTree 'EmptyTree (Node ('S ('S ('S ('S ('S 'Z))))) [Int]) 'EmptyTree) (Node ('S ('S ('S ('S ('S ('S 'Z)))))) [Char]) ('ForkTree 'EmptyTree (Node ('S ('S ('S ('S ('S ('S ('S 'Z))))))) [Bool]) 'EmptyTree))))
t8 = insertBST (mkNode p7 SD) t7

lala :: [Char]
lala = lookupBST p6 t8

-- | Error: key p1 ('S 'Z) is not in the tree t8
-- err = lookupBST p1 t8
