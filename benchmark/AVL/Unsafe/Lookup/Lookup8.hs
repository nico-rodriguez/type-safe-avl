{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}


module AVL.Unsafe.Lookup.Lookup8 (v8, main) where

import           Data.Time.Clock             (diffUTCTime, getCurrentTime)
import           Data.Tree.AVL.Unsafe        (lookupAVL)
import           Prelude                     (IO, putStrLn, return, seq, show, (++), (^))
import           AVL.Unsafe.Example.Example8 (t8)


v8 = lookupAVL (4^8) t8

main :: IO ()
main = do seq t8 (return ())
          t0 <- getCurrentTime
          seq v8 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
