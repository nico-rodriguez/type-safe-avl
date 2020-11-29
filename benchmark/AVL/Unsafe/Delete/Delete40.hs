{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module AVL.Unsafe.Delete.Delete40 (e40, main) where


import           Data.Time.Clock            (diffUTCTime, getCurrentTime)
import           Data.Tree.AVL.Unsafe       (deleteAVL)
import           Prelude                    (IO, putStrLn, return, seq,
                                            show, (++), ($))
import           AVL.Unsafe.Insert.Insert40 (t40)


e40 = deleteAVL 39 $ deleteAVL 38 $ deleteAVL 37 $ deleteAVL 36 $ deleteAVL 35 $ deleteAVL 34 $ deleteAVL 33 $ deleteAVL 32 $ deleteAVL 31 $ deleteAVL 30 $
      deleteAVL 29 $ deleteAVL 28 $ deleteAVL 27 $ deleteAVL 26 $ deleteAVL 25 $ deleteAVL 24 $ deleteAVL 23 $ deleteAVL 22 $ deleteAVL 21 $ deleteAVL 20 $
      deleteAVL 19 $ deleteAVL 18 $ deleteAVL 17 $ deleteAVL 16 $ deleteAVL 15 $ deleteAVL 14 $ deleteAVL 13 $ deleteAVL 12 $ deleteAVL 11 $ deleteAVL 10 $
      deleteAVL 9 $ deleteAVL 8 $ deleteAVL 7 $ deleteAVL 6 $ deleteAVL 5 $ deleteAVL 4 $ deleteAVL 3 $ deleteAVL 2 $ deleteAVL 1 $ deleteAVL 0 t40

main :: IO ()
main = do t0 <- getCurrentTime
          seq t40 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
