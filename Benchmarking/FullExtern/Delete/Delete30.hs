{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module Benchmarking.FullExtern.Delete.Delete30 (e30, main) where


import           Benchmarking.FullExtern.Operations (ProofIsAVL (proofIsAVL), ProofIsBST (proofIsBST))
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Extern.AVLOperations               (Deletable (delete))
import           Data.Type.Equality                 (gcastWith)
import           Extern.AVLProofs                   (AVL (AVL))
import           Prelude                        (Bool (False), IO, putStrLn,
                                                 return, seq, show, (++), ($))
import           Node                               (mkNode)
import           Benchmarking.FullExtern.Insert.Insert30 (t30)


e30 = case t30 of
  AVL t30' ->
    gcastWith (proofIsAVL t) $ gcastWith (proofIsBST t) $ AVL t
      where
        t = delete (Proxy::Proxy 29) $ delete (Proxy::Proxy 28) $ delete (Proxy::Proxy 27) $ delete (Proxy::Proxy 26) $ delete (Proxy::Proxy 25) $ delete (Proxy::Proxy 24) $ delete (Proxy::Proxy 23) $ delete (Proxy::Proxy 22) $ delete (Proxy::Proxy 21) $ delete (Proxy::Proxy 20) $
          delete (Proxy::Proxy 19) $ delete (Proxy::Proxy 18) $ delete (Proxy::Proxy 17) $ delete (Proxy::Proxy 16) $ delete (Proxy::Proxy 15) $ delete (Proxy::Proxy 14) $ delete (Proxy::Proxy 13) $ delete (Proxy::Proxy 12) $ delete (Proxy::Proxy 11) $ delete (Proxy::Proxy 10) $
            delete (Proxy::Proxy 9) $ delete (Proxy::Proxy 8) $ delete (Proxy::Proxy 7) $ delete (Proxy::Proxy 6) $ delete (Proxy::Proxy 5) $ delete (Proxy::Proxy 4) $ delete (Proxy::Proxy 3) $ delete (Proxy::Proxy 2) $ delete (Proxy::Proxy 1) $ delete (Proxy::Proxy 0) $ t30'

main :: IO ()
main = do seq t30 (return ())
          t0 <- getCurrentTime
          seq e30 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
