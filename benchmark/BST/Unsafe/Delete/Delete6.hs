{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module BST.Unsafe.Delete.Delete6 (e6, main) where


import           Data.Time.Clock           (diffUTCTime, getCurrentTime)
import           Data.Tree.BST.Unsafe      (deleteBST)
import           Prelude                   (IO, putStrLn, return, seq,
                                           show, (++), flip, (^))
import           BST.Unsafe.Insert.Insert6 (t6)
import           Data.Foldable             (foldl')


e6 = foldl' deleteBST' t6 [0..2^11]
    where
        deleteBST' = flip deleteBST

main :: IO ()
main = do t0 <- getCurrentTime
          seq e6 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
