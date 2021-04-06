{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module BST.Unsafe.Delete.Delete10 (e10, main) where


import           Data.Time.Clock            (diffUTCTime, getCurrentTime)
import           Data.Tree.BST.Unsafe       (deleteBST)
import           Prelude                    (IO, putStrLn, return, seq,
                                            show, (++), flip, (^))
import           BST.Unsafe.Example.Example10 (t10)


e10 = deleteBST (2^15) t10

main :: IO ()
main = do t0 <- getCurrentTime
          seq e10 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
