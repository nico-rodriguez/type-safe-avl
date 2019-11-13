{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Benchmarking.FullExtern.Operations (InsertN, deleteN, insertN)
import           Data.Proxy                     (Proxy (Proxy))
import           Extern.AVL                     (AVL (AVL), lookupAVL)
import           ITree                          (Tree (EmptyTree), ITree (EmptyITree))
import           Prelude                        (Bool (False), IO, putStrLn,
                                                 return, seq, show, (++), Char)
import           Data.Time.Clock                (diffUTCTime, getCurrentTime)


t100 :: AVL (InsertN 100 'False 'EmptyTree)
t100 = insertN (Proxy::Proxy 100) (Proxy::Proxy 'False)
t200 :: AVL (InsertN 200 'False 'EmptyTree)
t200 = insertN (Proxy::Proxy 200) (Proxy::Proxy 'False)
t300 :: AVL (InsertN 300 'False 'EmptyTree)
t300 = insertN (Proxy::Proxy 300) (Proxy::Proxy 'False)
t400 :: AVL (InsertN 400 'False 'EmptyTree)
t400 = insertN (Proxy::Proxy 400) (Proxy::Proxy 'False)
e1 :: AVL 'EmptyTree
e1 = deleteN (Proxy::Proxy 100) t100
e2 :: AVL 'EmptyTree
e2 = deleteN (Proxy::Proxy 200) t200
e3 :: AVL 'EmptyTree
e3 = deleteN (Proxy::Proxy 300) t300
e4 :: AVL 'EmptyTree
e4 = deleteN (Proxy::Proxy 400) t400
v1 :: Char
v1 = lookupAVL (Proxy::Proxy 100) t100
v2 :: Char
v2 = lookupAVL (Proxy::Proxy 200) t200
v3 :: Char
v3 = lookupAVL (Proxy::Proxy 300) t300
v4 :: Char
v4 = lookupAVL (Proxy::Proxy 400) t400

main :: IO ()
main = do t0 <- getCurrentTime
          -- Insert
          seq t100 (return ())
          t1 <- getCurrentTime
          putStrLn ("Insert time (N=100): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq t200 (return ())
          t1 <- getCurrentTime
          putStrLn ("Insert time (N=200): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq t300 (return ())
          t1 <- getCurrentTime
          putStrLn ("Insert time (N=300): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq t400 (return ())
          t1 <- getCurrentTime
          putStrLn ("Insert time (N=400): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          -- Delete
          seq e1 (return ())
          t1 <- getCurrentTime
          putStrLn ("Delete time (N=100): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq e2 (return ())
          t1 <- getCurrentTime
          putStrLn ("Delete time (N=200): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq e3 (return ())
          t1 <- getCurrentTime
          putStrLn ("Delete time (N=300): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq e4 (return ())
          t1 <- getCurrentTime
          putStrLn ("Delete time (N=400): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          -- Lookup
          seq v1 (return ())
          t1 <- getCurrentTime
          putStrLn ("Lookup time (N=100): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq v2 (return ())
          t1 <- getCurrentTime
          putStrLn ("Lookup time (N=200): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq v3 (return ())
          t1 <- getCurrentTime
          putStrLn ("Lookup time (N=300): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq v4 (return ())
          t1 <- getCurrentTime
          putStrLn ("Lookup time (N=400): " ++ show (diffUTCTime t0 t1) ++ " seconds")
