{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}


module Benchmarking.Intern.Lookup.Lookup100 (v100, main) where

import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Intern.AVL                     (lookupAVL)
import           Prelude                        (IO, putStrLn, return, seq, show, (++))
import           Benchmarking.Intern.Insert.Insert100 (t100)


v100 = lookupAVL (Proxy::Proxy 0) t100

main :: IO ()
main = do seq t100 (return ())
          t0 <- getCurrentTime
          seq v100 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
