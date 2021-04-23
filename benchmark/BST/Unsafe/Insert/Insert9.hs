{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module BST.Unsafe.Insert.Insert9 (t9', main) where

import           Data.Time.Clock      (diffUTCTime,
                                       getCurrentTime)
import           Data.Tree.BST.Unsafe (emptyBST, insertBST)
import           Prelude              (IO, putStrLn, return, seq,
                                       show, (++), (^), (+))
import           BST.Unsafe.Example.Example9 (t9)


t9' = insertBST (2^14 + 1) 'a' t9

main :: IO ()
main = do t0 <- getCurrentTime
          seq t9' (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
