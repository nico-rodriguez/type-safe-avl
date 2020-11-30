{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}


module BST.Unsafe.Lookup.Lookup1 (v1, main) where

import           Data.Time.Clock             (diffUTCTime, getCurrentTime)
import           Data.Tree.BST.Unsafe        (lookupBST)
import           Prelude                     (IO, putStrLn, return, seq, show, (++), (^))
import           BST.Unsafe.Insert.Insert1 (t1)


v1 = lookupBST (2^6) t1

main :: IO ()
main = do seq t1 (return ())
          t0 <- getCurrentTime
          seq v1 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
