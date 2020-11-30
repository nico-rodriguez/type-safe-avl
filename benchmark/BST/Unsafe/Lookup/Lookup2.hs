{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}


module BST.Unsafe.Lookup.Lookup2 (v2, main) where

import           Data.Time.Clock             (diffUTCTime, getCurrentTime)
import           Data.Tree.BST.Unsafe        (lookupBST)
import           Prelude                     (IO, putStrLn, return, seq, show, (++), (^))
import           BST.Unsafe.Insert.Insert2 (t2)


v2 = lookupBST (2^7) t2

main :: IO ()
main = do seq t2 (return ())
          t0 <- getCurrentTime
          seq v2 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
