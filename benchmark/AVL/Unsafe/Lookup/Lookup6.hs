{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}


module AVL.Unsafe.Lookup.Lookup6 (v6, main) where

import           Data.Time.Clock             (diffUTCTime, getCurrentTime)
import           Data.Tree.AVL.Unsafe        (lookupAVL)
import           Prelude                     (IO, putStrLn, return, seq, show, (++), (^))
import           AVL.Unsafe.Insert.Insert6 (t6)


v6 = lookupAVL (2^11) t6

main :: IO ()
main = do seq t6 (return ())
          t0 <- getCurrentTime
          seq v6 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
