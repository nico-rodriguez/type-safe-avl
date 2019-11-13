{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Benchmarking.FullExtern.Operations (InsertN, deleteN, insertN)
import           Data.Proxy                     (Proxy (Proxy))
import           Extern.AVL                     (AVL (AVL), lookupAVL)
import           ITree                          (Tree (EmptyTree), ITree (EmptyITree))
import           Prelude                        (Bool (False), IO, putStrLn,
                                                 return, seq, show, (++), Char)
import           Data.Time.Clock                (diffUTCTime, getCurrentTime)


t10 :: AVL (InsertN 10 'False 'EmptyTree)
t10 = insertN (Proxy::Proxy 10) (Proxy::Proxy 'False)
t20 :: AVL (InsertN 20 'False 'EmptyTree)
t20 = insertN (Proxy::Proxy 20) (Proxy::Proxy 'False)
t30 :: AVL (InsertN 30 'False 'EmptyTree)
t30 = insertN (Proxy::Proxy 30) (Proxy::Proxy 'False)
t40 :: AVL (InsertN 40 'False 'EmptyTree)
t40 = insertN (Proxy::Proxy 40) (Proxy::Proxy 'False)
e1 :: AVL 'EmptyTree
e1 = deleteN (Proxy::Proxy 10) t10
e2 :: AVL 'EmptyTree
e2 = deleteN (Proxy::Proxy 20) t20
e3 :: AVL 'EmptyTree
e3 = deleteN (Proxy::Proxy 30) t30
e4 :: AVL 'EmptyTree
e4 = deleteN (Proxy::Proxy 40) t40
v1 :: Char
v1 = lookupAVL (Proxy::Proxy 10) t10
v2 :: Char
v2 = lookupAVL (Proxy::Proxy 20) t20
v3 :: Char
v3 = lookupAVL (Proxy::Proxy 30) t30
v4 :: Char
v4 = lookupAVL (Proxy::Proxy 40) t40

main :: IO ()
main = do t0 <- getCurrentTime
          -- Insert
          seq t10 (return ())
          t1 <- getCurrentTime
          putStrLn ("Insert time (N=10): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq t20 (return ())
          t1 <- getCurrentTime
          putStrLn ("Insert time (N=20): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq t30 (return ())
          t1 <- getCurrentTime
          putStrLn ("Insert time (N=30): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq t40 (return ())
          t1 <- getCurrentTime
          putStrLn ("Insert time (N=40): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          -- Delete
          seq e1 (return ())
          t1 <- getCurrentTime
          putStrLn ("Delete time (N=10): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq e2 (return ())
          t1 <- getCurrentTime
          putStrLn ("Delete time (N=20): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq e3 (return ())
          t1 <- getCurrentTime
          putStrLn ("Delete time (N=30): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq e4 (return ())
          t1 <- getCurrentTime
          putStrLn ("Delete time (N=40): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          -- Lookup
          seq v1 (return ())
          t1 <- getCurrentTime
          putStrLn ("Lookup time (N=10): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq v2 (return ())
          t1 <- getCurrentTime
          putStrLn ("Lookup time (N=20): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq v3 (return ())
          t1 <- getCurrentTime
          putStrLn ("Lookup time (N=30): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq v4 (return ())
          t1 <- getCurrentTime
          putStrLn ("Lookup time (N=40): " ++ show (diffUTCTime t0 t1) ++ " seconds")
