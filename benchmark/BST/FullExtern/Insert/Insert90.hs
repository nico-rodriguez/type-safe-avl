{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module BST.FullExtern.Insert.Insert90 (t90', main) where

import           Data.Proxy               (Proxy (Proxy))
import           Data.Time.Clock          (diffUTCTime,
                                           getCurrentTime)
import           Data.Tree.Node           (mkNode)
import           Data.Tree.BST.FullExtern (ITree (EmptyITree), insert, mkBST)
import           Prelude                  (IO, putStrLn, return, seq,
                                           show, ($), (++))
import           BST.FullExtern.Example.Example90 (t90)


t90' = mkBST t
  where
    t = insert (mkNode (Proxy::Proxy 90) 'a') t90

main :: IO ()
main = do t0 <- getCurrentTime
          seq t90' (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
