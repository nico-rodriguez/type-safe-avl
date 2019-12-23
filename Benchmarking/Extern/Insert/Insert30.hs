{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module Benchmarking.Extern.Insert.Insert30 (t30, main) where

import           Data.Proxy      (Proxy (Proxy))
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           Extern.AVL      (emptyAVL, insertAVL)
import           Node            (mkNode)
import           Prelude         (IO, putStrLn, return, seq, show, ($), (++))


t30 = insertAVL (mkNode (Proxy::Proxy 29) 'a') $ insertAVL (mkNode (Proxy::Proxy 28) 'a') $ insertAVL (mkNode (Proxy::Proxy 27) 'a') $ insertAVL (mkNode (Proxy::Proxy 26) 'a') $ insertAVL (mkNode (Proxy::Proxy 25) 'a') $ insertAVL (mkNode (Proxy::Proxy 24) 'a') $ insertAVL (mkNode (Proxy::Proxy 23) 'a') $ insertAVL (mkNode (Proxy::Proxy 22) 'a') $ insertAVL (mkNode (Proxy::Proxy 21) 'a') $ insertAVL (mkNode (Proxy::Proxy 20) 'a') $
  insertAVL (mkNode (Proxy::Proxy 19) 'a') $ insertAVL (mkNode (Proxy::Proxy 18) 'a') $ insertAVL (mkNode (Proxy::Proxy 17) 'a') $ insertAVL (mkNode (Proxy::Proxy 16) 'a') $ insertAVL (mkNode (Proxy::Proxy 15) 'a') $ insertAVL (mkNode (Proxy::Proxy 14) 'a') $ insertAVL (mkNode (Proxy::Proxy 13) 'a') $ insertAVL (mkNode (Proxy::Proxy 12) 'a') $ insertAVL (mkNode (Proxy::Proxy 11) 'a') $ insertAVL (mkNode (Proxy::Proxy 10) 'a') $
    insertAVL (mkNode (Proxy::Proxy 9) 'a') $ insertAVL (mkNode (Proxy::Proxy 8) 'a') $ insertAVL (mkNode (Proxy::Proxy 7) 'a') $ insertAVL (mkNode (Proxy::Proxy 6) 'a') $ insertAVL (mkNode (Proxy::Proxy 5) 'a') $ insertAVL (mkNode (Proxy::Proxy 4) 'a') $ insertAVL (mkNode (Proxy::Proxy 3) 'a') $ insertAVL (mkNode (Proxy::Proxy 2) 'a') $ insertAVL (mkNode (Proxy::Proxy 1) 'a') $ insertAVL (mkNode (Proxy::Proxy 0) 'a') $ emptyAVL

main :: IO ()
main = do t0 <- getCurrentTime
          seq t30 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
