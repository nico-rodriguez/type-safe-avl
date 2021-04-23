{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}


module AVL.Unsafe.Lookup.Lookup10 (v10, main) where

import           Data.Time.Clock             (diffUTCTime, getCurrentTime)
import           Data.Tree.AVL.Unsafe        (lookupAVL)
import           Prelude                     (IO, putStrLn, return, seq, show, (++), (^))
import           AVL.Unsafe.Example.Example10 (t10)


v10 = lookupAVL (5^10) t10

main :: IO ()
main = do seq t10 (return ())
          t0 <- getCurrentTime
          seq v10 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
