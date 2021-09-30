{-# OPTIONS_HADDOCK hide, prune #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module AVL.FullExtern.Example.Example20 (t20, main) where

import           Data.Proxy               (Proxy (Proxy))
import           Data.Time.Clock          (diffUTCTime,
                                           getCurrentTime)
import           Data.Tree.Node           (mkNode)
import           Data.Tree.AVL.FullExtern (ITree (EmptyITree), insert, mkAVL)
import           Prelude                  (IO, putStrLn, return, seq,
                                           show, ($), (++))


t20 = mkAVL t
  where
    t = insert (mkNode (Proxy::Proxy 19) 'a') $ insert (mkNode (Proxy::Proxy 18) 'a') $ insert (mkNode (Proxy::Proxy 17) 'a') $ insert (mkNode (Proxy::Proxy 16) 'a') $ insert (mkNode (Proxy::Proxy 15) 'a') $ insert (mkNode (Proxy::Proxy 14) 'a') $ insert (mkNode (Proxy::Proxy 13) 'a') $ insert (mkNode (Proxy::Proxy 12) 'a') $ insert (mkNode (Proxy::Proxy 11) 'a') $ insert (mkNode (Proxy::Proxy 10) 'a') $
        insert (mkNode (Proxy::Proxy 9) 'a') $ insert (mkNode (Proxy::Proxy 8) 'a') $ insert (mkNode (Proxy::Proxy 7) 'a') $ insert (mkNode (Proxy::Proxy 6) 'a') $ insert (mkNode (Proxy::Proxy 5) 'a') $ insert (mkNode (Proxy::Proxy 4) 'a') $ insert (mkNode (Proxy::Proxy 3) 'a') $ insert (mkNode (Proxy::Proxy 2) 'a') $ insert (mkNode (Proxy::Proxy 1) 'a') $ insert (mkNode (Proxy::Proxy 0) 'a') EmptyITree

main :: IO ()
main = do t0 <- getCurrentTime
          seq t20 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
