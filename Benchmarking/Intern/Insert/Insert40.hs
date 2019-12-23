{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module Benchmarking.Intern.Insert.Insert40 (t40, main) where

import           Data.Proxy      (Proxy (Proxy))
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           Intern.AVL      (emptyAVL, insertAVL)
import           Node            (mkNode)
import           Prelude         (IO, putStrLn, return, seq, show, ($), (++))


t40 = insertAVL (mkNode (Proxy::Proxy 39) 'a') $ insertAVL (mkNode (Proxy::Proxy 38) 'a') $ insertAVL (mkNode (Proxy::Proxy 37) 'a') $ insertAVL (mkNode (Proxy::Proxy 36) 'a') $ insertAVL (mkNode (Proxy::Proxy 35) 'a') $ insertAVL (mkNode (Proxy::Proxy 34) 'a') $ insertAVL (mkNode (Proxy::Proxy 33) 'a') $ insertAVL (mkNode (Proxy::Proxy 32) 'a') $ insertAVL (mkNode (Proxy::Proxy 31) 'a') $ insertAVL (mkNode (Proxy::Proxy 30) 'a') $
  insertAVL (mkNode (Proxy::Proxy 29) 'a') $ insertAVL (mkNode (Proxy::Proxy 28) 'a') $ insertAVL (mkNode (Proxy::Proxy 27) 'a') $ insertAVL (mkNode (Proxy::Proxy 26) 'a') $ insertAVL (mkNode (Proxy::Proxy 25) 'a') $ insertAVL (mkNode (Proxy::Proxy 24) 'a') $ insertAVL (mkNode (Proxy::Proxy 23) 'a') $ insertAVL (mkNode (Proxy::Proxy 22) 'a') $ insertAVL (mkNode (Proxy::Proxy 21) 'a') $ insertAVL (mkNode (Proxy::Proxy 20) 'a') $
    insertAVL (mkNode (Proxy::Proxy 19) 'a') $ insertAVL (mkNode (Proxy::Proxy 18) 'a') $ insertAVL (mkNode (Proxy::Proxy 17) 'a') $ insertAVL (mkNode (Proxy::Proxy 16) 'a') $ insertAVL (mkNode (Proxy::Proxy 15) 'a') $ insertAVL (mkNode (Proxy::Proxy 14) 'a') $ insertAVL (mkNode (Proxy::Proxy 13) 'a') $ insertAVL (mkNode (Proxy::Proxy 12) 'a') $ insertAVL (mkNode (Proxy::Proxy 11) 'a') $ insertAVL (mkNode (Proxy::Proxy 10) 'a') $
      insertAVL (mkNode (Proxy::Proxy 9) 'a') $ insertAVL (mkNode (Proxy::Proxy 8) 'a') $ insertAVL (mkNode (Proxy::Proxy 7) 'a') $ insertAVL (mkNode (Proxy::Proxy 6) 'a') $ insertAVL (mkNode (Proxy::Proxy 5) 'a') $ insertAVL (mkNode (Proxy::Proxy 4) 'a') $ insertAVL (mkNode (Proxy::Proxy 3) 'a') $ insertAVL (mkNode (Proxy::Proxy 2) 'a') $ insertAVL (mkNode (Proxy::Proxy 1) 'a') $ insertAVL (mkNode (Proxy::Proxy 0) 'a') $ emptyAVL

main :: IO ()
main = do t0 <- getCurrentTime
          seq t40 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
