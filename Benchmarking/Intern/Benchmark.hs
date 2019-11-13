{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Benchmarking.Intern.Operations (InsertN, deleteN, insertN)
import           Benchmarking.Utils             (secDiff)
import           Data.Proxy                     (Proxy (Proxy))
import           Intern.AVL                     (AVL (EmptyAVL), lookupAVL)
import           ITree                          (Tree (EmptyTree))
import           Prelude                        (Bool (False), IO, putStrLn,
                                                 return, seq, show, (++), Char)
import           System.Time                    (getClockTime)


t500 :: AVL (InsertN 500 'False 'EmptyTree)
t500 = insertN (Proxy::Proxy 500) (Proxy::Proxy 'False) EmptyAVL
t1000 :: AVL (InsertN 1000 'False 'EmptyTree)
t1000 = insertN (Proxy::Proxy 1000) (Proxy::Proxy 'False) EmptyAVL
t1500 :: AVL (InsertN 1500 'False 'EmptyTree)
t1500 = insertN (Proxy::Proxy 1500) (Proxy::Proxy 'False) EmptyAVL
t2000 :: AVL (InsertN 2000 'False 'EmptyTree)
t2000 = insertN (Proxy::Proxy 2000) (Proxy::Proxy 'False) EmptyAVL
e1 :: AVL 'EmptyTree
e1 = deleteN (Proxy::Proxy 500) t500
e2 :: AVL 'EmptyTree
e2 = deleteN (Proxy::Proxy 1000) t1000
e3 :: AVL 'EmptyTree
e3 = deleteN (Proxy::Proxy 1500) t1500
e4 :: AVL 'EmptyTree
e4 = deleteN (Proxy::Proxy 2000) t2000
v1 :: Char
v1 = lookupAVL (Proxy::Proxy 500) t500
v2 :: Char
v2 = lookupAVL (Proxy::Proxy 1000) t1000
v3 :: Char
v3 = lookupAVL (Proxy::Proxy 1500) t1500
v4 :: Char
v4 = lookupAVL (Proxy::Proxy 2000) t2000

main :: IO ()
main = do t0 <- getClockTime
          -- Insert
          seq t500 (return ())
          t1 <- getClockTime
          putStrLn ("Insert time (N=500): " ++ show (secDiff t0 t1) ++ " seconds")
          seq t1000 (return ())
          t1 <- getClockTime
          putStrLn ("Insert time (N=1000): " ++ show (secDiff t0 t1) ++ " seconds")
          seq t1500 (return ())
          t1 <- getClockTime
          putStrLn ("Insert time (N=1500): " ++ show (secDiff t0 t1) ++ " seconds")
          seq t2000 (return ())
          t1 <- getClockTime
          putStrLn ("Insert time (N=2000): " ++ show (secDiff t0 t1) ++ " seconds")
          -- Delete
          seq e1 (return ())
          t1 <- getClockTime
          putStrLn ("Delete time (N=500): " ++ show (secDiff t0 t1) ++ " seconds")
          seq e2 (return ())
          t1 <- getClockTime
          putStrLn ("Delete time (N=1000): " ++ show (secDiff t0 t1) ++ " seconds")
          seq e3 (return ())
          t1 <- getClockTime
          putStrLn ("Delete time (N=1500): " ++ show (secDiff t0 t1) ++ " seconds")
          seq e4 (return ())
          t1 <- getClockTime
          putStrLn ("Delete time (N=2000): " ++ show (secDiff t0 t1) ++ " seconds")
          -- Lookup
          seq v1 (return ())
          t1 <- getClockTime
          putStrLn ("Lookup time (N=500): " ++ show (secDiff t0 t1) ++ " seconds")
          seq v2 (return ())
          t1 <- getClockTime
          putStrLn ("Lookup time (N=1000): " ++ show (secDiff t0 t1) ++ " seconds")
          seq v3 (return ())
          t1 <- getClockTime
          putStrLn ("Lookup time (N=1500): " ++ show (secDiff t0 t1) ++ " seconds")
          seq v4 (return ())
          t1 <- getClockTime
          putStrLn ("Lookup time (N=2000): " ++ show (secDiff t0 t1) ++ " seconds")
