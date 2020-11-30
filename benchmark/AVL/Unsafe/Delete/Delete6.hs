{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module AVL.Unsafe.Delete.Delete6 (e6, main) where


import           Data.Time.Clock           (diffUTCTime, getCurrentTime)
import           Data.Tree.AVL.Unsafe      (deleteAVL)
import           Prelude                   (IO, putStrLn, return, seq,
                                           show, (++), flip, (^))
import           AVL.Unsafe.Insert.Insert6 (t6)
import           Data.Foldable             (foldl')


e6 = foldl' deleteAVL' t6 [0..2^11]
    where
        deleteAVL' = flip deleteAVL

main :: IO ()
main = do t0 <- getCurrentTime
          seq e6 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
