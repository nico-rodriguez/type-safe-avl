{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module BST.Unsafe.Delete.Delete3 (e3, main) where


import           Data.Time.Clock           (diffUTCTime, getCurrentTime)
import           Data.Tree.BST.Unsafe      (deleteBST)
import           Prelude                   (IO, putStrLn, return, seq,
                                           show, (++), flip, (^))
import           BST.Unsafe.Insert.Insert3 (t3)
import           Data.Foldable             (foldl')


e3 = foldl' deleteBST' t3 [0..2^8]
    where
        deleteBST' = flip deleteBST

main :: IO ()
main = do t0 <- getCurrentTime
          seq e3 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
