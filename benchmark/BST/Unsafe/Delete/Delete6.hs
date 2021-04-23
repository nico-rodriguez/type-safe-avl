{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module BST.Unsafe.Delete.Delete6 (e6, main) where


import           Data.Time.Clock           (diffUTCTime, getCurrentTime)
import           Data.Tree.BST.Unsafe      (deleteBST)
import           Prelude                   (IO, putStrLn, return, seq,
                                           show, (++), (^))
import           BST.Unsafe.Example.Example6 (t6)


e6 = deleteBST (10^11) t6

main :: IO ()
main = do t0 <- getCurrentTime
          seq e6 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
