{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

module AVL.Unsafe.Delete.Delete10 (e10, main) where


import           Data.Time.Clock            (diffUTCTime, getCurrentTime)
import           Data.Tree.AVL.Unsafe       (deleteAVL)
import           Prelude                    (IO, putStrLn, return, seq,
                                            show, (++), ($))
import           AVL.Unsafe.Insert.Insert10 (t10)


e10 = deleteAVL 9 $ deleteAVL 8 $ deleteAVL 7 $ deleteAVL 6 $ deleteAVL 5 $ deleteAVL 4 $ deleteAVL 3 $ deleteAVL 2 $ deleteAVL 1 $ deleteAVL 0 t10

main :: IO ()
main = do t0 <- getCurrentTime
          seq t10 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
