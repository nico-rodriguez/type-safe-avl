{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module Benchmarking.FullExtern.Delete.Delete10 (e10, main) where


import           Benchmarking.FullExtern.Operations (ProofIsAVL (proofIsAVL), ProofIsBST (proofIsBST))
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Extern.AVLOperations               (Deletable (delete))
import           Data.Type.Equality                 (gcastWith)
import           Extern.AVLProofs                   (AVL (AVL))
import           Prelude                        (Bool (False), IO, putStrLn,
                                                 return, seq, show, (++), ($))
import           Node                               (mkNode)
import           Benchmarking.FullExtern.Insert.Insert10 (t10)


e10 = case t10 of
  AVL t10' ->
    gcastWith (proofIsAVL t) $ gcastWith (proofIsBST t) $ AVL t
      where
        t = delete (Proxy::Proxy 9) $ delete (Proxy::Proxy 8) $ delete (Proxy::Proxy 7) $ delete (Proxy::Proxy 6) $ delete (Proxy::Proxy 5) $ delete (Proxy::Proxy 4) $ delete (Proxy::Proxy 3) $ delete (Proxy::Proxy 2) $ delete (Proxy::Proxy 1) $ delete (Proxy::Proxy 0) $ t10'

main :: IO ()
main = do seq t10 (return ())
          t0 <- getCurrentTime
          seq e10 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
