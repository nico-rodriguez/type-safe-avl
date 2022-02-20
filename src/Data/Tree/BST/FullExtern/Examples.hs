{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeFamilies       #-}

{-# LANGUAGE Safe               #-}

{-# OPTIONS_GHC -Wno-missing-signatures          #-}
{-# OPTIONS_GHC -Wno-missing-export-lists        #-}
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}


module Data.Tree.BST.FullExtern.Examples where

import           Data.Proxy               (Proxy (Proxy))
import           Data.Tree.BST.FullExtern (mkBST, BST (BST), ITree (EmptyITree),
                                           delete, insert, lookup)
import           Data.Tree.ITree          (ITree(EmptyITree,ForkITree))
import           Data.Tree.Node           (mkNode)
import           Prelude                  (Bool (False, True), Float, Int, ($))




-- | Proxies for the node keys
p0 = Proxy :: Proxy 0
p1 = Proxy :: Proxy 1
p2 = Proxy :: Proxy 2
p3 = Proxy :: Proxy 3
p4 = Proxy :: Proxy 4
p5 = Proxy :: Proxy 5
p6 = Proxy :: Proxy 6
p7 = Proxy :: Proxy 7


emptyTree = EmptyITree

-- | Insert several values in a row and check the BST invariants at the end
bst = mkBST t
  where
    t = insert (mkNode p4 'f') $ insert (mkNode p2 (4::Int)) $
        insert (mkNode p6 "lala") $ insert (mkNode p3 True) $
        insert (mkNode p5 ([1,2,3]::[Int])) $ insert (mkNode p0 (1.8::Float)) $
        insert (mkNode p7 [False]) emptyTree

-- bstError = mkBST $
--   ForkITree (ForkITree EmptyITree (mkNode p7 4) EmptyITree)
--             (mkNode p4 'f')
--             EmptyITree

-- | For performing a lookup, it's necessary to take the ITree @t@ out of the BST constructor
l1 = case bst of
    BST t _ -> lookup p6 t

-- -- | Error: key 1 is not in the tree bst
-- err = case bst of
--     BST t _ -> lookup p1 t

-- | Delete several values in a row and check the BST invariants at the end
bst2 = case bst of
  BST t _ -> mkBST t'
    where
      t' =  delete p7 $ delete p4 $ delete p1 $ delete p0 $
            delete p2 $ delete p6 $ delete p5 $ delete p3 t
