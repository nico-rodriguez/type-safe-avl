{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module AVL.Unsafe.Delete.Delete7 (e7, main) where


import           Data.Time.Clock           (diffUTCTime, getCurrentTime)
import           Data.Tree.AVL.Unsafe      (deleteAVL)
import           Prelude                   (IO, putStrLn, return, seq,
                                           show, (++), flip, (^))
import           AVL.Unsafe.Insert.Insert7 (t7)
import           Data.Foldable             (foldl')


e7 = foldl' deleteAVL' t7 [0..2^12]
    where
        deleteAVL' = flip deleteAVL

main :: IO ()
main = do t0 <- getCurrentTime
          seq e7 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
