{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module BST.FullExtern.Insert.Insert20 (t20', main) where

import           Data.Proxy               (Proxy (Proxy))
import           Data.Time.Clock          (diffUTCTime,
                                           getCurrentTime)
import           Data.Tree.Node           (mkNode)
import           Data.Tree.BST.FullExtern (ITree (EmptyITree), insert, mkBST)
import           Prelude                  (IO, putStrLn, return, seq,
                                           show, ($), (++))
import           BST.FullExtern.Example.Example20 (t20)


t20' = mkBST t
  where
    t = insert (mkNode (Proxy::Proxy 20) 'a') t20

main :: IO ()
main = do t0 <- getCurrentTime
          seq t20' (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
