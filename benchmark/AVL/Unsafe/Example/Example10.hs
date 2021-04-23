{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module AVL.Unsafe.Example.Example10 (t10, main) where

import           Data.Time.Clock      (diffUTCTime,
                                       getCurrentTime)
import           Data.Tree.AVL.Unsafe (emptyAVL, insertAVL)
import           Prelude              (IO, putStrLn, return, seq,
                                       show, (++), (^))
import           Data.Foldable        (foldl')


t10 = foldl' insertAVL' emptyAVL [0..2^15]
        where
                insertAVL' tree key = insertAVL key 'a' tree

main :: IO ()
main = do t0 <- getCurrentTime
          seq t10 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
