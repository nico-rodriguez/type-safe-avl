{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}


module Benchmarking.Intern.Lookup.Lookup80 (v80, main) where

import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Intern.AVL                     (lookupAVL)
import           Prelude                        (IO, putStrLn, return, seq, show, (++))
import           Benchmarking.Intern.Insert.Insert80 (t80)


v80 = lookupAVL (Proxy::Proxy 0) t80

main :: IO ()
main = do seq t80 (return ())
          t0 <- getCurrentTime
          seq v80 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
