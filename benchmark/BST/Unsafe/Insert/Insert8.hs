{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module BST.Unsafe.Insert.Insert8 (t8', main) where

import           Data.Time.Clock      (diffUTCTime,
                                       getCurrentTime)
import           Data.Tree.BST.Unsafe (emptyBST, insertBST)
import           Prelude              (IO, putStrLn, return, seq,
                                       show, (++), (^))
import           BST.Unsafe.Example.Example8 (t8)


t8' = insertBST (2^13 + 1) 'a' t8

main :: IO ()
main = do t0 <- getCurrentTime
          seq t8' (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
