{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}


module BST.Unsafe.Lookup.Lookup5 (v5, main) where

import           Data.Time.Clock             (diffUTCTime, getCurrentTime)
import           Data.Tree.BST.Unsafe        (lookupBST)
import           Prelude                     (IO, putStrLn, return, seq, show, (++), (^))
import           BST.Unsafe.Example.Example5 (t5)


v5 = lookupBST (4^5) t5

main :: IO ()
main = do seq t5 (return ())
          t0 <- getCurrentTime
          seq v5 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
