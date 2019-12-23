{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}


module Benchmarking.Intern.Lookup.Lookup70 (v70, main) where

import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Intern.AVL                     (lookupAVL)
import           Prelude                        (IO, putStrLn, return, seq, show, (++))
import           Benchmarking.Intern.Insert.Insert70 (t70)


v70 = lookupAVL (Proxy::Proxy 0) t70

main :: IO ()
main = do seq t70 (return ())
          t0 <- getCurrentTime
          seq v70 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
