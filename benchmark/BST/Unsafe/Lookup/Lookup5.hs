{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}


module BST.Unsafe.Lookup.Lookup5 (v5, main) where

import           Data.Time.Clock             (diffUTCTime, getCurrentTime)
import           Data.Tree.BST.Unsafe        (lookupBST)
import           Prelude                     (IO, putStrLn, return, seq, show, (++), (^))
import           BST.Unsafe.Insert.Insert5 (t5)


v5 = lookupBST (2^10) t5

main :: IO ()
main = do seq t5 (return ())
          t0 <- getCurrentTime
          seq v5 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
