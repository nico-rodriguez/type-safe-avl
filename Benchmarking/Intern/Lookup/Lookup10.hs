{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}


module Benchmarking.Intern.Lookup.Lookup10 (v10, main) where

import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Intern.AVL                     (lookupAVL)
import           Prelude                        (IO, putStrLn, return, seq, show, (++))
import           Benchmarking.Intern.Insert.Insert10 (t10)


v10 = lookupAVL (Proxy::Proxy 0) t10

main :: IO ()
main = do seq t10 (return ())
          t0 <- getCurrentTime
          seq v10 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
