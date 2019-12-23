{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module Benchmarking.Intern.Insert.Insert90 (t90, main) where

import           Data.Proxy      (Proxy (Proxy))
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           Intern.AVL      (emptyAVL, insertAVL)
import           Node            (mkNode)
import           Prelude         (IO, putStrLn, return, seq, show, ($), (++))


t90 = insertAVL (mkNode (Proxy::Proxy 89) 'a') $ insertAVL (mkNode (Proxy::Proxy 88) 'a') $ insertAVL (mkNode (Proxy::Proxy 87) 'a') $ insertAVL (mkNode (Proxy::Proxy 86) 'a') $ insertAVL (mkNode (Proxy::Proxy 85) 'a') $ insertAVL (mkNode (Proxy::Proxy 84) 'a') $ insertAVL (mkNode (Proxy::Proxy 83) 'a') $ insertAVL (mkNode (Proxy::Proxy 82) 'a') $ insertAVL (mkNode (Proxy::Proxy 81) 'a') $ insertAVL (mkNode (Proxy::Proxy 80) 'a') $
  insertAVL (mkNode (Proxy::Proxy 79) 'a') $ insertAVL (mkNode (Proxy::Proxy 78) 'a') $ insertAVL (mkNode (Proxy::Proxy 77) 'a') $ insertAVL (mkNode (Proxy::Proxy 76) 'a') $ insertAVL (mkNode (Proxy::Proxy 75) 'a') $ insertAVL (mkNode (Proxy::Proxy 74) 'a') $ insertAVL (mkNode (Proxy::Proxy 73) 'a') $ insertAVL (mkNode (Proxy::Proxy 72) 'a') $ insertAVL (mkNode (Proxy::Proxy 71) 'a') $ insertAVL (mkNode (Proxy::Proxy 70) 'a') $
    insertAVL (mkNode (Proxy::Proxy 69) 'a') $ insertAVL (mkNode (Proxy::Proxy 68) 'a') $ insertAVL (mkNode (Proxy::Proxy 67) 'a') $ insertAVL (mkNode (Proxy::Proxy 66) 'a') $ insertAVL (mkNode (Proxy::Proxy 65) 'a') $ insertAVL (mkNode (Proxy::Proxy 64) 'a') $ insertAVL (mkNode (Proxy::Proxy 63) 'a') $ insertAVL (mkNode (Proxy::Proxy 62) 'a') $ insertAVL (mkNode (Proxy::Proxy 61) 'a') $ insertAVL (mkNode (Proxy::Proxy 60) 'a') $
      insertAVL (mkNode (Proxy::Proxy 59) 'a') $ insertAVL (mkNode (Proxy::Proxy 58) 'a') $ insertAVL (mkNode (Proxy::Proxy 57) 'a') $ insertAVL (mkNode (Proxy::Proxy 56) 'a') $ insertAVL (mkNode (Proxy::Proxy 55) 'a') $ insertAVL (mkNode (Proxy::Proxy 54) 'a') $ insertAVL (mkNode (Proxy::Proxy 53) 'a') $ insertAVL (mkNode (Proxy::Proxy 52) 'a') $ insertAVL (mkNode (Proxy::Proxy 51) 'a') $ insertAVL (mkNode (Proxy::Proxy 50) 'a') $
        insertAVL (mkNode (Proxy::Proxy 49) 'a') $ insertAVL (mkNode (Proxy::Proxy 48) 'a') $ insertAVL (mkNode (Proxy::Proxy 47) 'a') $ insertAVL (mkNode (Proxy::Proxy 46) 'a') $ insertAVL (mkNode (Proxy::Proxy 45) 'a') $ insertAVL (mkNode (Proxy::Proxy 44) 'a') $ insertAVL (mkNode (Proxy::Proxy 43) 'a') $ insertAVL (mkNode (Proxy::Proxy 42) 'a') $ insertAVL (mkNode (Proxy::Proxy 41) 'a') $ insertAVL (mkNode (Proxy::Proxy 40) 'a') $
          insertAVL (mkNode (Proxy::Proxy 39) 'a') $ insertAVL (mkNode (Proxy::Proxy 38) 'a') $ insertAVL (mkNode (Proxy::Proxy 37) 'a') $ insertAVL (mkNode (Proxy::Proxy 36) 'a') $ insertAVL (mkNode (Proxy::Proxy 35) 'a') $ insertAVL (mkNode (Proxy::Proxy 34) 'a') $ insertAVL (mkNode (Proxy::Proxy 33) 'a') $ insertAVL (mkNode (Proxy::Proxy 32) 'a') $ insertAVL (mkNode (Proxy::Proxy 31) 'a') $ insertAVL (mkNode (Proxy::Proxy 30) 'a') $
            insertAVL (mkNode (Proxy::Proxy 29) 'a') $ insertAVL (mkNode (Proxy::Proxy 28) 'a') $ insertAVL (mkNode (Proxy::Proxy 27) 'a') $ insertAVL (mkNode (Proxy::Proxy 26) 'a') $ insertAVL (mkNode (Proxy::Proxy 25) 'a') $ insertAVL (mkNode (Proxy::Proxy 24) 'a') $ insertAVL (mkNode (Proxy::Proxy 23) 'a') $ insertAVL (mkNode (Proxy::Proxy 22) 'a') $ insertAVL (mkNode (Proxy::Proxy 21) 'a') $ insertAVL (mkNode (Proxy::Proxy 20) 'a') $
              insertAVL (mkNode (Proxy::Proxy 19) 'a') $ insertAVL (mkNode (Proxy::Proxy 18) 'a') $ insertAVL (mkNode (Proxy::Proxy 17) 'a') $ insertAVL (mkNode (Proxy::Proxy 16) 'a') $ insertAVL (mkNode (Proxy::Proxy 15) 'a') $ insertAVL (mkNode (Proxy::Proxy 14) 'a') $ insertAVL (mkNode (Proxy::Proxy 13) 'a') $ insertAVL (mkNode (Proxy::Proxy 12) 'a') $ insertAVL (mkNode (Proxy::Proxy 11) 'a') $ insertAVL (mkNode (Proxy::Proxy 10) 'a') $
                insertAVL (mkNode (Proxy::Proxy 9) 'a') $ insertAVL (mkNode (Proxy::Proxy 8) 'a') $ insertAVL (mkNode (Proxy::Proxy 7) 'a') $ insertAVL (mkNode (Proxy::Proxy 6) 'a') $ insertAVL (mkNode (Proxy::Proxy 5) 'a') $ insertAVL (mkNode (Proxy::Proxy 4) 'a') $ insertAVL (mkNode (Proxy::Proxy 3) 'a') $ insertAVL (mkNode (Proxy::Proxy 2) 'a') $ insertAVL (mkNode (Proxy::Proxy 1) 'a') $ insertAVL (mkNode (Proxy::Proxy 0) 'a') $ emptyAVL

main :: IO ()
main = do t0 <- getCurrentTime
          seq t90 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
