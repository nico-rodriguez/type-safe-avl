{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeFamilies       #-}

{-# LANGUAGE Safe               #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}


module Data.Tree.BST.FullExtern.Examples where

import           Data.Proxy               (Proxy (Proxy))
import           Data.Tree.BST.FullExtern (mkBST, BST (BST), ITree (EmptyITree),
                                           delete, insert, lookup)
import           Data.Tree.Node           (mkNode)
import           Prelude                  (Bool (False, True), Float, Int, ($))


p0 = Proxy :: Proxy 0
p1 = Proxy :: Proxy 1
p2 = Proxy :: Proxy 2
p3 = Proxy :: Proxy 3
p4 = Proxy :: Proxy 4
p5 = Proxy :: Proxy 5
p6 = Proxy :: Proxy 6
p7 = Proxy :: Proxy 7


emptyTree = EmptyITree

bst = mkBST t
  where
    t = insert (mkNode p4 'f') $ insert (mkNode p2 (4::Int)) $
        insert (mkNode p6 "lala") $ insert (mkNode p3 True) $
        insert (mkNode p5 ([1,2,3]::[Int])) $ insert (mkNode p0 (1.8::Float)) $
        insert (mkNode p7 [False]) emptyTree

l1 = case bst of
    BST t _ -> lookup p6 t

-- | Error: key 1 is not in the tree bst
-- err = case bst of
--     BST t _ -> lookup p1 t

bst2 = case bst of
  BST t _ -> mkBST t'
    where
      t' =  delete p7 $ delete p4 $ delete p1 $ delete p0 $
            delete p2 $ delete p6 $ delete p5 $ delete p3 t
