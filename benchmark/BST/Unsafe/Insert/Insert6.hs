{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module BST.Unsafe.Insert.Insert6 (t6', main) where

import           Data.Time.Clock      (diffUTCTime,
                                       getCurrentTime)
import           Data.Tree.BST.Unsafe (emptyBST, insertBST)
import           Prelude              (IO, putStrLn, return, seq,
                                       show, (++), (^), (+))
import           BST.Unsafe.Example.Example6 (t6)


t6' = insertBST (2^11 + 1) 'a' t6

main :: IO ()
main = do t0 <- getCurrentTime
          seq t6' (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
