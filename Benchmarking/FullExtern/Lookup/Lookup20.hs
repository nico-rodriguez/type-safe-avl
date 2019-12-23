{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}


module Benchmarking.Extern.Lookup.Lookup20 (v20, main) where

import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Extern.AVL                     (AVL (AVL))
import           Extern.BSTOperations               (Lookupable (lookup))
import           Prelude                        (IO, putStrLn, return, seq, show, (++))
import           Benchmarking.Extern.Insert.Insert20 (t20)


v20 = case t20 of
  AVL t20' -> lookup (Proxy::Proxy 0) t20'

main :: IO ()
main = do seq t20 (return ())
          t0 <- getCurrentTime
          seq v20 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
