{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module Benchmarking.Intern.Insert.Insert10 (t10, main) where

import           Data.Proxy      (Proxy (Proxy))
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           Intern.AVL      (emptyAVL, insertAVL)
import           Node            (mkNode)
import           Prelude         (IO, putStrLn, return, seq, show, ($), (++))


t10 = insertAVL (mkNode (Proxy::Proxy 9) 'a') $ insertAVL (mkNode (Proxy::Proxy 8) 'a') $ insertAVL (mkNode (Proxy::Proxy 7) 'a') $ insertAVL (mkNode (Proxy::Proxy 6) 'a') $ insertAVL (mkNode (Proxy::Proxy 5) 'a') $ insertAVL (mkNode (Proxy::Proxy 4) 'a') $ insertAVL (mkNode (Proxy::Proxy 3) 'a') $ insertAVL (mkNode (Proxy::Proxy 2) 'a') $ insertAVL (mkNode (Proxy::Proxy 1) 'a') $ insertAVL (mkNode (Proxy::Proxy 0) 'a') $ emptyAVL

main :: IO ()
main = do t0 <- getCurrentTime
          seq t10 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
