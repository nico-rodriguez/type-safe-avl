{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module AVL.FullExtern.Insert.Insert40 (t40', main) where

import           Data.Proxy               (Proxy (Proxy))
import           Data.Time.Clock          (diffUTCTime,
                                           getCurrentTime)
import           Data.Tree.Node           (mkNode)
import           Data.Tree.AVL.FullExtern (ITree (EmptyITree), insert, mkAVL)
import           Prelude                  (IO, putStrLn, return, seq,
                                           show, ($), (++))
import           AVL.FullExtern.Example.Example40 (t40)


t40' = mkAVL t
  where
    t = insert (mkNode (Proxy::Proxy 40) 'a') t40

main :: IO ()
main = do t0 <- getCurrentTime
          seq t40' (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
