{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module BST.Unsafe.Insert.Insert2 (t2, main) where

import           Data.Time.Clock      (diffUTCTime,
                                       getCurrentTime)
import           Data.Tree.BST.Unsafe (emptyBST, insertBST)
import           Prelude              (IO, putStrLn, return, seq,
                                       show, (++), (^))
import           Data.Foldable        (foldl')


t2 = foldl' insertBST' emptyBST [0..2^7]
        where
                insertBST' tree key = insertBST key 'a' tree

main :: IO ()
main = do t0 <- getCurrentTime
          seq t2 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
