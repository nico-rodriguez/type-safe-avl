{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module AVL.Unsafe.Insert.Insert9 (t9, main) where

import           Data.Time.Clock      (diffUTCTime,
                                       getCurrentTime)
import           Data.Tree.AVL.Unsafe (emptyAVL, insertAVL)
import           Prelude              (IO, putStrLn, return, seq,
                                       show, (++), (^))
import           Data.Foldable        (foldl')


t9 = foldl' insertAVL' emptyAVL [0..2^14]
        where
                insertAVL' tree key = insertAVL key 'a' tree

main :: IO ()
main = do t0 <- getCurrentTime
          seq t9 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
