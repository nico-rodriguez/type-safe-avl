{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module BST.Unsafe.Delete.Delete10 (e10, main) where


import           Data.Time.Clock            (diffUTCTime, getCurrentTime)
import           Data.Tree.BST.Unsafe       (deleteBST)
import           Prelude                    (IO, putStrLn, return, seq,
                                            show, (++), flip, (^))
import           BST.Unsafe.Insert.Insert10 (t10)
import           Data.Foldable              (foldl')


e10 = foldl' deleteBST' t10 [0..2^15]
    where
        deleteBST' = flip deleteBST

main :: IO ()
main = do t0 <- getCurrentTime
          seq e10 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
