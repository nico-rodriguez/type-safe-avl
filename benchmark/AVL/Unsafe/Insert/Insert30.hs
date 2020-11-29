{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module AVL.Unsafe.Insert.Insert30 (t30, main) where

import           Data.Time.Clock      (diffUTCTime,
                                       getCurrentTime)
import           Data.Tree.AVL.Unsafe (emptyAVL, insertAVL)
import           Prelude              (IO, putStrLn, return, seq,
                                       show, ($), (++))


t30 = insertAVL 29 'a' $ insertAVL 28 'a' $ insertAVL 27 'a' $ insertAVL 26 'a' $ insertAVL 25 'a' $ insertAVL 24 'a' $ insertAVL 23 'a' $ insertAVL 22 'a' $ insertAVL 21 'a' $ insertAVL 20 'a' $
      insertAVL 19 'a' $ insertAVL 18 'a' $ insertAVL 17 'a' $ insertAVL 16 'a' $ insertAVL 15 'a' $ insertAVL 14 'a' $ insertAVL 13 'a' $ insertAVL 12 'a' $ insertAVL 11 'a' $ insertAVL 10 'a' $
      insertAVL 9 'a' $ insertAVL 8 'a' $ insertAVL 7 'a' $ insertAVL 6 'a' $ insertAVL 5 'a' $ insertAVL 4 'a' $ insertAVL 3 'a' $ insertAVL 2 'a' $ insertAVL 1 'a' $ insertAVL 0 'a' emptyAVL

main :: IO ()
main = do t0 <- getCurrentTime
          seq t30 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
