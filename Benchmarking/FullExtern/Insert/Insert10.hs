{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module Benchmarking.FullExtern.Insert.Insert10 (t10, main) where

import           Benchmarking.FullExtern.Operations (ProofIsAVL (proofIsAVL),
                                                     ProofIsBST (proofIsBST))
import           Data.Proxy                         (Proxy (Proxy))
import           Data.Time.Clock                    (diffUTCTime,
                                                     getCurrentTime)
import           Data.Type.Equality                 (gcastWith)
import           Extern.AVLProofs                   (AVL (AVL))
import           ITree                              (ITree (EmptyITree))
import           Extern.AVLOperations               (Insertable (insert))
import           Node                               (mkNode)
import           Prelude                            (IO, putStrLn, return, seq,
                                                     show, ($), (++))


t10 = gcastWith (proofIsAVL t) $ gcastWith (proofIsBST t) $ AVL t
  where
    t =  insert (mkNode (Proxy::Proxy 9) 'a') $ insert (mkNode (Proxy::Proxy 8) 'a') $ insert (mkNode (Proxy::Proxy 7) 'a') $ insert (mkNode (Proxy::Proxy 6) 'a') $ insert (mkNode (Proxy::Proxy 5) 'a') $ insert (mkNode (Proxy::Proxy 4) 'a') $ insert (mkNode (Proxy::Proxy 3) 'a') $ insert (mkNode (Proxy::Proxy 2) 'a') $ insert (mkNode (Proxy::Proxy 1) 'a') $ insert (mkNode (Proxy::Proxy 0) 'a') $ EmptyITree

main :: IO ()
main = do t0 <- getCurrentTime
          seq t10 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
