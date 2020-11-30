{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module BST.Unsafe.Delete.Delete4 (e4, main) where


import           Data.Time.Clock           (diffUTCTime, getCurrentTime)
import           Data.Tree.BST.Unsafe      (deleteBST)
import           Prelude                   (IO, putStrLn, return, seq,
                                           show, (++), flip, (^))
import           BST.Unsafe.Insert.Insert4 (t4)
import           Data.Foldable             (foldl')


e4 = foldl' deleteBST' t4 [0..2^9]
    where
        deleteBST' = flip deleteBST

main :: IO ()
main = do t0 <- getCurrentTime
          seq e4 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
