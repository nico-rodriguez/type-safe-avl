{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}


module BST.Unsafe.Lookup.Lookup3 (v3, main) where

import           Data.Time.Clock             (diffUTCTime, getCurrentTime)
import           Data.Tree.BST.Unsafe        (lookupBST)
import           Prelude                     (IO, putStrLn, return, seq, show, (++), (^))
import           BST.Unsafe.Example.Example3 (t3)


v3 = lookupBST (2^8) t3

main :: IO ()
main = do seq t3 (return ())
          t0 <- getCurrentTime
          seq v3 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
