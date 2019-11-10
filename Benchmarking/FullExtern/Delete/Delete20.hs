{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Benchmarking.FullExtern.Operations (InsertN, deleteN, insertN)
import           Benchmarking.Utils                 (secDiff)
import           Data.Proxy                         (Proxy (Proxy))
import           Extern.AVL                         (AVL ())
import           ITree                              (Tree (EmptyTree))
import           Prelude                            (Bool (False), IO, putStrLn,
                                                     return, seq, show, (++))
import           System.Time                        (getClockTime)


t20 :: AVL (InsertN 20)
t20 = insertN (Proxy::Proxy 20) (Proxy::Proxy 'False)
e :: AVL 'EmptyTree
e = deleteN (Proxy::Proxy 20) t20

main :: IO ()
main = do seq t20 (return ())
          t0 <- getClockTime
          seq e (return ())
          t1 <- getClockTime
          putStrLn ("Time: " ++ show (secDiff t0 t1) ++ " seconds")
