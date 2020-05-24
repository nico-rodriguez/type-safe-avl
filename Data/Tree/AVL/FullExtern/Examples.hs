{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}


module Data.Tree.AVL.FullExtern.Examples where

import           Data.Proxy (Proxy (Proxy))
import           Data.Type.Equality   (gcastWith)
import           Data.Tree.AVL.FullExtern (delete, ITree(EmptyITree), insert, lookup, AVL(AVL),
                                ProofIsAVL(proofIsAVL))
import           Data.Tree.Node (mkNode)
import           Data.Tree.BST.FullExtern (BST(BST), ProofIsBST(proofIsBST))
import           Prelude (Int, Float, ($), Bool (True,False))


p0 = Proxy :: Proxy 0
p1 = Proxy :: Proxy 1
p2 = Proxy :: Proxy 2
p3 = Proxy :: Proxy 3
p4 = Proxy :: Proxy 4
p5 = Proxy :: Proxy 5
p6 = Proxy :: Proxy 6
p7 = Proxy :: Proxy 7


emptyTree = EmptyITree

-- | Test Balanced Binary Tree
avl = gcastWith (proofIsAVL t) $ gcastWith (proofIsBST t) $ AVL t
    where
        t = insert (mkNode p4 'f') $ insert (mkNode p2 (4::Int)) $ insert (mkNode p6 "lala") $ insert (mkNode p3 True) $ insert (mkNode p5 ([1,2,3]::[Int])) $ insert (mkNode p0 (1.8::Float)) $ insert (mkNode p7 [False]) emptyTree

l1' = case avl of
    AVL t -> lookup p6 t

-- | Error: key 1 is not in the tree avl
-- err = case avl of
--     AVL t -> lookup p1 t

avlt2 = case avl of
    AVL t -> gcastWith (proofIsAVL t') $ gcastWith (proofIsBST t') $ AVL t'
                where
                    t' = delete p7 $ delete p4 $ delete p1 $ delete p0 $ delete p2 $ delete p6 $ delete p5 $ delete p3 t
