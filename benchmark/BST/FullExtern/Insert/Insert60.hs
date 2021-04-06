{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module BST.FullExtern.Insert.Insert60 (t60', main) where

import           Data.Proxy               (Proxy (Proxy))
import           Data.Time.Clock          (diffUTCTime,
                                           getCurrentTime)
import           Data.Tree.Node           (mkNode)
import           Data.Tree.BST.FullExtern (ITree (EmptyITree), insert, mkBST)
import           Prelude                  (IO, putStrLn, return, seq,
                                           show, ($), (++))
import           BST.FullExtern.Example.Example60 (t60)


t60' = mkBST t
  where
    t = insert (mkNode (Proxy::Proxy 60) 'a') t60

main :: IO ()
main = do t0 <- getCurrentTime
          seq t60' (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
