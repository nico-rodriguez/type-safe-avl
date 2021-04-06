{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module BST.Unsafe.Insert.Insert1 (t1', main) where

import           Data.Time.Clock      (diffUTCTime,
                                       getCurrentTime)
import           Data.Tree.BST.Unsafe (emptyBST, insertBST)
import           Prelude              (IO, putStrLn, return, seq,
                                       show, (++), (^))
import           BST.Unsafe.Example.Example1 (t1)


t1' = insertBST (2^6 + 1) 'a' t1

main :: IO ()
main = do t0 <- getCurrentTime
          seq t1' (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
