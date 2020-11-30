{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module AVL.Unsafe.Delete.Delete1 (e1, main) where


import           Data.Time.Clock           (diffUTCTime, getCurrentTime)
import           Data.Tree.AVL.Unsafe      (deleteAVL)
import           Prelude                   (IO, putStrLn, return, seq,
                                            show, (++), flip, (^))
import           AVL.Unsafe.Insert.Insert1 (t1)
import           Data.Foldable             (foldl')


e1 = foldl' deleteAVL' t1 [0..2^6]
    where
        deleteAVL' = flip deleteAVL

main :: IO ()
main = do t0 <- getCurrentTime
          seq e1 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
