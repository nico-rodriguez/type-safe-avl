{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}


module AVL.Unsafe.Lookup.Lookup9 (v9, main) where

import           Data.Time.Clock             (diffUTCTime, getCurrentTime)
import           Data.Tree.AVL.Unsafe        (lookupAVL)
import           Prelude                     (IO, putStrLn, return, seq, show, (++), (^))
import           AVL.Unsafe.Example.Example9 (t9)


v9 = lookupAVL (4^9) t9

main :: IO ()
main = do seq t9 (return ())
          t0 <- getCurrentTime
          seq v9 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
