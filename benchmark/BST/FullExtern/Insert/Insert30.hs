{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module BST.FullExtern.Insert.Insert30 (t30, main) where

import           Data.Proxy               (Proxy (Proxy))
import           Data.Time.Clock          (diffUTCTime,
                                           getCurrentTime)
import           Data.Tree.Node           (mkNode)
import           Data.Tree.BST.FullExtern (ITree (EmptyITree), insert, mkBST)
import           Prelude                  (IO, putStrLn, return, seq,
                                           show, ($), (++))


t30 = mkBST t
  where
    t = insert (mkNode (Proxy::Proxy 29) 'a') $ insert (mkNode (Proxy::Proxy 28) 'a') $ insert (mkNode (Proxy::Proxy 27) 'a') $ insert (mkNode (Proxy::Proxy 26) 'a') $ insert (mkNode (Proxy::Proxy 25) 'a') $ insert (mkNode (Proxy::Proxy 24) 'a') $ insert (mkNode (Proxy::Proxy 23) 'a') $ insert (mkNode (Proxy::Proxy 22) 'a') $ insert (mkNode (Proxy::Proxy 21) 'a') $ insert (mkNode (Proxy::Proxy 20) 'a') $
        insert (mkNode (Proxy::Proxy 19) 'a') $ insert (mkNode (Proxy::Proxy 18) 'a') $ insert (mkNode (Proxy::Proxy 17) 'a') $ insert (mkNode (Proxy::Proxy 16) 'a') $ insert (mkNode (Proxy::Proxy 15) 'a') $ insert (mkNode (Proxy::Proxy 14) 'a') $ insert (mkNode (Proxy::Proxy 13) 'a') $ insert (mkNode (Proxy::Proxy 12) 'a') $ insert (mkNode (Proxy::Proxy 11) 'a') $ insert (mkNode (Proxy::Proxy 10) 'a') $
        insert (mkNode (Proxy::Proxy 9) 'a') $ insert (mkNode (Proxy::Proxy 8) 'a') $ insert (mkNode (Proxy::Proxy 7) 'a') $ insert (mkNode (Proxy::Proxy 6) 'a') $ insert (mkNode (Proxy::Proxy 5) 'a') $ insert (mkNode (Proxy::Proxy 4) 'a') $ insert (mkNode (Proxy::Proxy 3) 'a') $ insert (mkNode (Proxy::Proxy 2) 'a') $ insert (mkNode (Proxy::Proxy 1) 'a') $ insert (mkNode (Proxy::Proxy 0) 'a') EmptyITree

main :: IO ()
main = do t0 <- getCurrentTime
          seq t30 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
