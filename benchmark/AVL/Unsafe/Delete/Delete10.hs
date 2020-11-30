{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module AVL.Unsafe.Delete.Delete10 (e10, main) where


import           Data.Time.Clock            (diffUTCTime, getCurrentTime)
import           Data.Tree.AVL.Unsafe       (deleteAVL)
import           Prelude                    (IO, putStrLn, return, seq,
                                            show, (++), flip, (^))
import           AVL.Unsafe.Insert.Insert10 (t10)
import           Data.Foldable              (foldl')


e10 = foldl' deleteAVL' t10 [0..2^15]
    where
        deleteAVL' = flip deleteAVL

main :: IO ()
main = do t0 <- getCurrentTime
          seq e10 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
