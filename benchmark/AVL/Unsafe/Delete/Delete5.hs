{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module AVL.Unsafe.Delete.Delete5 (e5, main) where


import           Data.Time.Clock           (diffUTCTime, getCurrentTime)
import           Data.Tree.AVL.Unsafe      (deleteAVL)
import           Prelude                   (IO, putStrLn, return, seq,
                                           show, (++), (^))
import           AVL.Unsafe.Example.Example5 (t5)


e5 = deleteAVL (4^5) t5

main :: IO ()
main = do t0 <- getCurrentTime
          seq e5 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
