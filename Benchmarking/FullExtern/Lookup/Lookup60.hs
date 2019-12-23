{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}


module Benchmarking.Extern.Lookup.Lookup60 (v60, main) where

import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Extern.AVL                     (AVL (AVL))
import           Extern.BSTOperations               (Lookupable (lookup))
import           Prelude                        (IO, putStrLn, return, seq, show, (++))
import           Benchmarking.Extern.Insert.Insert60 (t60)


v60 = case t60 of
  AVL t60' -> lookup (Proxy::Proxy 0) t60'

main :: IO ()
main = do seq t60 (return ())
          t0 <- getCurrentTime
          seq v60 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
