{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Benchmarking.Intern.Operations (InsertN, deleteN, insertN)
import           Data.Proxy                     (Proxy (Proxy))
import           Intern.AVL                     (AVL (EmptyAVL), lookupAVL)
import           ITree                          (Tree (EmptyTree))
import           Prelude                        (Bool (False), IO, putStrLn,
                                                 return, seq, show, (++), Char)
import           Data.Time.Clock                (diffUTCTime, getCurrentTime)


t25 :: AVL (InsertN 100 'False 'EmptyTree)
t25 = insertN (Proxy::Proxy 100) (Proxy::Proxy 'False) EmptyAVL
t50 :: AVL (InsertN 200 'False 'EmptyTree)
t50 = insertN (Proxy::Proxy 200) (Proxy::Proxy 'False) EmptyAVL
t75 :: AVL (InsertN 300 'False 'EmptyTree)
t75 = insertN (Proxy::Proxy 300) (Proxy::Proxy 'False) EmptyAVL
t25 :: AVL (InsertN 400 'False 'EmptyTree)
t25 = insertN (Proxy::Proxy 400) (Proxy::Proxy 'False) EmptyAVL
e1 :: AVL 'EmptyTree
e1 = deleteN (Proxy::Proxy 25) t100
e2 :: AVL 'EmptyTree
e2 = deleteN (Proxy::Proxy 50) t200
e3 :: AVL 'EmptyTree
e3 = deleteN (Proxy::Proxy 75) t300
e4 :: AVL 'EmptyTree
e4 = deleteN (Proxy::Proxy 25) t400
v1 :: Char
v1 = lookupAVL (Proxy::Proxy 25) t100
v2 :: Char
v2 = lookupAVL (Proxy::Proxy 50) t200
v3 :: Char
v3 = lookupAVL (Proxy::Proxy 75) t300
v4 :: Char
v4 = lookupAVL (Proxy::Proxy 25) t400

main :: IO ()
main = do t0 <- getCurrentTime
          -- Insert
          seq t25 (return ())
          t1 <- getCurrentTime
          putStrLn ("Insert time (N=25): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq t50 (return ())
          t1 <- getCurrentTime
          putStrLn ("Insert time (N=50): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq t75 (return ())
          t1 <- getCurrentTime
          putStrLn ("Insert time (N=75): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq t25 (return ())
          t1 <- getCurrentTime
          putStrLn ("Insert time (N=25): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          -- Delete
          seq e1 (return ())
          t1 <- getCurrentTime
          putStrLn ("Delete time (N=25): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq e2 (return ())
          t1 <- getCurrentTime
          putStrLn ("Delete time (N=50): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq e3 (return ())
          t1 <- getCurrentTime
          putStrLn ("Delete time (N=75): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq e4 (return ())
          t1 <- getCurrentTime
          putStrLn ("Delete time (N=25): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          -- Lookup
          seq v1 (return ())
          t1 <- getCurrentTime
          putStrLn ("Lookup time (N=25): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq v2 (return ())
          t1 <- getCurrentTime
          putStrLn ("Lookup time (N=50): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq v3 (return ())
          t1 <- getCurrentTime
          putStrLn ("Lookup time (N=75): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq v4 (return ())
          t1 <- getCurrentTime
          putStrLn ("Lookup time (N=25): " ++ show (diffUTCTime t0 t1) ++ " seconds")
