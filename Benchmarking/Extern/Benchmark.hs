{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Benchmarking.Extern.Operations (InsertN, deleteN, insertN)
import           Benchmarking.Utils             (secDiff)
import           Data.Proxy                     (Proxy (Proxy))
import           Extern.AVL                     (AVL (AVL), lookupAVL)
import           ITree                          (Tree (EmptyTree), ITree (EmptyITree))
import           Prelude                        (Bool (False), IO, putStrLn,
                                                 return, seq, show, (++), Char)
import           System.Time                    (getClockTime)


t50 :: AVL (InsertN 50 'False 'EmptyTree)
t50 = insertN (Proxy::Proxy 50) (Proxy::Proxy 'False) (AVL EmptyITree)
t100 :: AVL (InsertN 100 'False 'EmptyTree)
t100 = insertN (Proxy::Proxy 100) (Proxy::Proxy 'False) (AVL EmptyITree)
t150 :: AVL (InsertN 150 'False 'EmptyTree)
t150 = insertN (Proxy::Proxy 150) (Proxy::Proxy 'False) (AVL EmptyITree)
t200 :: AVL (InsertN 200 'False 'EmptyTree)
t200 = insertN (Proxy::Proxy 200) (Proxy::Proxy 'False) (AVL EmptyITree)
e1 :: AVL 'EmptyTree
e1 = deleteN (Proxy::Proxy 50) t50
e2 :: AVL 'EmptyTree
e2 = deleteN (Proxy::Proxy 100) t100
e3 :: AVL 'EmptyTree
e3 = deleteN (Proxy::Proxy 150) t150
e4 :: AVL 'EmptyTree
e4 = deleteN (Proxy::Proxy 200) t200
v1 :: Char
v1 = lookupAVL (Proxy::Proxy 50) t50
v2 :: Char
v2 = lookupAVL (Proxy::Proxy 100) t100
v3 :: Char
v3 = lookupAVL (Proxy::Proxy 150) t150
v4 :: Char
v4 = lookupAVL (Proxy::Proxy 200) t200

main :: IO ()
main = do t0 <- getClockTime
          -- Insert
          seq t50 (return ())
          t1 <- getClockTime
          putStrLn ("Insert time (N=50): " ++ show (secDiff t0 t1) ++ " seconds")
          seq t100 (return ())
          t1 <- getClockTime
          putStrLn ("Insert time (N=100): " ++ show (secDiff t0 t1) ++ " seconds")
          seq t150 (return ())
          t1 <- getClockTime
          putStrLn ("Insert time (N=150): " ++ show (secDiff t0 t1) ++ " seconds")
          seq t200 (return ())
          t1 <- getClockTime
          putStrLn ("Insert time (N=200): " ++ show (secDiff t0 t1) ++ " seconds")
          -- Delete
          seq e1 (return ())
          t1 <- getClockTime
          putStrLn ("Delete time (N=50): " ++ show (secDiff t0 t1) ++ " seconds")
          seq e2 (return ())
          t1 <- getClockTime
          putStrLn ("Delete time (N=100): " ++ show (secDiff t0 t1) ++ " seconds")
          seq e3 (return ())
          t1 <- getClockTime
          putStrLn ("Delete time (N=150): " ++ show (secDiff t0 t1) ++ " seconds")
          seq e4 (return ())
          t1 <- getClockTime
          putStrLn ("Delete time (N=200): " ++ show (secDiff t0 t1) ++ " seconds")
          -- Lookup
          seq v1 (return ())
          t1 <- getClockTime
          putStrLn ("Lookup time (N=50): " ++ show (secDiff t0 t1) ++ " seconds")
          seq v2 (return ())
          t1 <- getClockTime
          putStrLn ("Lookup time (N=100): " ++ show (secDiff t0 t1) ++ " seconds")
          seq v3 (return ())
          t1 <- getClockTime
          putStrLn ("Lookup time (N=150): " ++ show (secDiff t0 t1) ++ " seconds")
          seq v4 (return ())
          t1 <- getClockTime
          putStrLn ("Lookup time (N=200): " ++ show (secDiff t0 t1) ++ " seconds")
