{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module AVL.Unsafe.Insert.Insert70 (t70, main) where

import           Data.Time.Clock      (diffUTCTime,
                                       getCurrentTime)
import           Data.Tree.AVL.Unsafe (emptyAVL, insertAVL)
import           Prelude              (IO, putStrLn, return, seq,
                                       show, ($), (++))


t70 = insertAVL 69 'a' $ insertAVL 68 'a' $ insertAVL 67 'a' $ insertAVL 66 'a' $ insertAVL 65 'a' $ insertAVL 64 'a' $ insertAVL 63 'a' $ insertAVL 62 'a' $ insertAVL 61 'a' $ insertAVL 60 'a' $
      insertAVL 59 'a' $ insertAVL 58 'a' $ insertAVL 57 'a' $ insertAVL 56 'a' $ insertAVL 55 'a' $ insertAVL 54 'a' $ insertAVL 53 'a' $ insertAVL 52 'a' $ insertAVL 51 'a' $ insertAVL 50 'a' $
      insertAVL 49 'a' $ insertAVL 48 'a' $ insertAVL 47 'a' $ insertAVL 46 'a' $ insertAVL 45 'a' $ insertAVL 44 'a' $ insertAVL 43 'a' $ insertAVL 42 'a' $ insertAVL 41 'a' $ insertAVL 40 'a' $
      insertAVL 39 'a' $ insertAVL 38 'a' $ insertAVL 37 'a' $ insertAVL 36 'a' $ insertAVL 35 'a' $ insertAVL 34 'a' $ insertAVL 33 'a' $ insertAVL 32 'a' $ insertAVL 31 'a' $ insertAVL 30 'a' $
      insertAVL 29 'a' $ insertAVL 28 'a' $ insertAVL 27 'a' $ insertAVL 26 'a' $ insertAVL 25 'a' $ insertAVL 24 'a' $ insertAVL 23 'a' $ insertAVL 22 'a' $ insertAVL 21 'a' $ insertAVL 20 'a' $
      insertAVL 19 'a' $ insertAVL 18 'a' $ insertAVL 17 'a' $ insertAVL 16 'a' $ insertAVL 15 'a' $ insertAVL 14 'a' $ insertAVL 13 'a' $ insertAVL 12 'a' $ insertAVL 11 'a' $ insertAVL 10 'a' $
      insertAVL 9 'a' $ insertAVL 8 'a' $ insertAVL 7 'a' $ insertAVL 6 'a' $ insertAVL 5 'a' $ insertAVL 4 'a' $ insertAVL 3 'a' $ insertAVL 2 'a' $ insertAVL 1 'a' $ insertAVL 0 'a' emptyAVL

main :: IO ()
main = do t0 <- getCurrentTime
          seq t70 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
