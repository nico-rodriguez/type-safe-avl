{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module AVL.Unsafe.Delete.Delete3 (e3, main) where


import           Data.Time.Clock           (diffUTCTime, getCurrentTime)
import           Data.Tree.AVL.Unsafe      (deleteAVL)
import           Prelude                   (IO, putStrLn, return, seq,
                                           show, (++), flip, (^))
import           AVL.Unsafe.Insert.Insert3 (t3)
import           Data.Foldable             (foldl')


e3 = foldl' deleteAVL' t3 [0..2^8]
    where
        deleteAVL' = flip deleteAVL

main :: IO ()
main = do t0 <- getCurrentTime
          seq e3 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
