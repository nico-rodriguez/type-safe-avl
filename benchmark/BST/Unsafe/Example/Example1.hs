{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module BST.Unsafe.Example.Example1 (t1, main) where

import           Data.Time.Clock      (diffUTCTime,
                                       getCurrentTime)
import           Data.Tree.BST.Unsafe (emptyBST, insertBST)
import           Prelude              (IO, putStrLn, return, seq,
                                       show, (++), (^))
import           Data.Foldable        (foldl')


t1 = foldl' insertBST' emptyBST [0..2^6]
        where
                insertBST' tree key = insertBST key 'a' tree

main :: IO ()
main = do t0 <- getCurrentTime
          seq t1 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
