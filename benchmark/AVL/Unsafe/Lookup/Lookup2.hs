{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}


module AVL.Unsafe.Lookup.Lookup2 (v2, main) where

import           Data.Time.Clock             (diffUTCTime, getCurrentTime)
import           Data.Tree.AVL.Unsafe        (lookupAVL)
import           Prelude                     (IO, putStrLn, return, seq, show, (++), (^))
import           AVL.Unsafe.Example.Example2 (t2)


v2 = lookupAVL (2^7) t2

main :: IO ()
main = do seq t2 (return ())
          t0 <- getCurrentTime
          seq v2 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
