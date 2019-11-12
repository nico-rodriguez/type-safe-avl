{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Benchmarking.Extern.Operations (InsertN, deleteN, insertN)
import           Benchmarking.Utils             (secDiff)
import           Data.Proxy                     (Proxy (Proxy))
import           Extern.AVL                     (AVL (AVL))
import           ITree                          (Tree (EmptyTree), ITree (EmptyITree))
import           Prelude                        (Bool (False), IO, putStrLn,
                                                 return, seq, show, (++))
import           System.Time                    (getClockTime)


t10 :: AVL (InsertN 10 'False 'EmptyTree)
t10 = insertN (Proxy::Proxy 10) (Proxy::Proxy 'False) (AVL EmptyITree)
e :: AVL 'EmptyTree
e = deleteN (Proxy::Proxy 10) t10

main :: IO ()
main = do seq t10 (return ())
          t0 <- getClockTime
          seq e (return ())
          t1 <- getClockTime
          putStrLn ("Time: " ++ show (secDiff t0 t1) ++ " seconds")
