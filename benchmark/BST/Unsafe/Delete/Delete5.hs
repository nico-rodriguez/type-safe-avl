{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module BST.Unsafe.Delete.Delete5 (e5, main) where


import           Data.Time.Clock           (diffUTCTime, getCurrentTime)
import           Data.Tree.BST.Unsafe      (deleteBST)
import           Prelude                   (IO, putStrLn, return, seq,
                                           show, (++), flip, (^))
import           BST.Unsafe.Insert.Insert5 (t5)
import           Data.Foldable             (foldl')


e5 = foldl' deleteBST' t5 [0..2^10]
    where
        deleteBST' = flip deleteBST

main :: IO ()
main = do t0 <- getCurrentTime
          seq e5 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
