{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module Benchmarking.FullExtern.Delete.Delete20 (e20, main) where


import           Benchmarking.FullExtern.Operations (ProofIsAVL (proofIsAVL), ProofIsBST (proofIsBST))
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Extern.AVLOperations               (Deletable (delete))
import           Data.Type.Equality                 (gcastWith)
import           Extern.AVLProofs                   (AVL (AVL))
import           Prelude                        (Bool (False), IO, putStrLn,
                                                 return, seq, show, (++), ($))
import           Node                               (mkNode)
import           Benchmarking.FullExtern.Insert.Insert20 (t20)


e20 = case t20 of
  AVL t20' ->
    gcastWith (proofIsAVL t) $ gcastWith (proofIsBST t) $ AVL t
      where
        t = delete (Proxy::Proxy 19) $ delete (Proxy::Proxy 18) $ delete (Proxy::Proxy 17) $ delete (Proxy::Proxy 16) $ delete (Proxy::Proxy 15) $ delete (Proxy::Proxy 14) $ delete (Proxy::Proxy 13) $ delete (Proxy::Proxy 12) $ delete (Proxy::Proxy 11) $ delete (Proxy::Proxy 10) $
          delete (Proxy::Proxy 9) $ delete (Proxy::Proxy 8) $ delete (Proxy::Proxy 7) $ delete (Proxy::Proxy 6) $ delete (Proxy::Proxy 5) $ delete (Proxy::Proxy 4) $ delete (Proxy::Proxy 3) $ delete (Proxy::Proxy 2) $ delete (Proxy::Proxy 1) $ delete (Proxy::Proxy 0) $ t20'

main :: IO ()
main = do seq t20 (return ())
          t0 <- getCurrentTime
          seq e20 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
