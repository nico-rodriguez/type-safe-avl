{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module BST.Unsafe.Delete.Delete2 (e2, main) where


import           Data.Time.Clock           (diffUTCTime, getCurrentTime)
import           Data.Tree.BST.Unsafe      (deleteBST)
import           Prelude                   (IO, putStrLn, return, seq,
                                           show, (++), (^))
import           BST.Unsafe.Example.Example2 (t2)


e2 = deleteBST (10^7) t2

main :: IO ()
main = do t0 <- getCurrentTime
          seq e2 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
