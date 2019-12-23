{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Data.Proxy                     (Proxy (Proxy))
import           Intern.AVL                     (lookupAVL)
import           Prelude                        (IO, putStrLn,
                                                 return, seq, show, (++))
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import Benchmarking.Intern.Insert.Insert10 (t10)
import Benchmarking.Intern.Insert.Insert20 (t20)
import Benchmarking.Intern.Insert.Insert30 (t30)
import Benchmarking.Intern.Insert.Insert40 (t40)
import Benchmarking.Intern.Insert.Insert50 (t50)
import Benchmarking.Intern.Insert.Insert60 (t60)
import Benchmarking.Intern.Insert.Insert70 (t70)
import Benchmarking.Intern.Insert.Insert80 (t80)
import Benchmarking.Intern.Insert.Insert90 (t90)
import Benchmarking.Intern.Insert.Insert100 (t100)
import Benchmarking.Intern.Delete.Delete10 (e10)
import Benchmarking.Intern.Delete.Delete20 (e20)
import Benchmarking.Intern.Delete.Delete30 (e30)
import Benchmarking.Intern.Delete.Delete40 (e40)
import Benchmarking.Intern.Delete.Delete50 (e50)
import Benchmarking.Intern.Delete.Delete60 (e60)
import Benchmarking.Intern.Delete.Delete70 (e70)
import Benchmarking.Intern.Delete.Delete80 (e80)
import Benchmarking.Intern.Delete.Delete90 (e90)
import Benchmarking.Intern.Delete.Delete100 (e100)


v10 = lookupAVL (Proxy::Proxy 0) t10
v20 = lookupAVL (Proxy::Proxy 0) t20
v30 = lookupAVL (Proxy::Proxy 0) t30
v40 = lookupAVL (Proxy::Proxy 0) t40
v50 = lookupAVL (Proxy::Proxy 0) t50
v60 = lookupAVL (Proxy::Proxy 0) t60
v70 = lookupAVL (Proxy::Proxy 0) t70
v80 = lookupAVL (Proxy::Proxy 0) t80
v90 = lookupAVL (Proxy::Proxy 0) t90
v10 = lookupAVL (Proxy::Proxy 0) t100

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
          seq t50 (return ())
          t1 <- getCurrentTime
          putStrLn ("Insert time (N=50): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq t60 (return ())
          t1 <- getCurrentTime
          putStrLn ("Insert time (N=60): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq t70 (return ())
          t1 <- getCurrentTime
          putStrLn ("Insert time (N=70): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq t80 (return ())
          t1 <- getCurrentTime
          putStrLn ("Insert time (N=80): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq t90 (return ())
          t1 <- getCurrentTime
          putStrLn ("Insert time (N=90): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq t100 (return ())
          t1 <- getCurrentTime
          putStrLn ("Insert time (N=100): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          -- Delete
          seq e10 (return ())
          t1 <- getCurrentTime
          putStrLn ("Delete time (N=10): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq e20 (return ())
          t1 <- getCurrentTime
          putStrLn ("Delete time (N=20): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq e30 (return ())
          t1 <- getCurrentTime
          putStrLn ("Delete time (N=30): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq e40 (return ())
          t1 <- getCurrentTime
          putStrLn ("Delete time (N=40): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq e50 (return ())
          t1 <- getCurrentTime
          putStrLn ("Delete time (N=50): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq e60 (return ())
          t1 <- getCurrentTime
          putStrLn ("Delete time (N=60): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq e70 (return ())
          t1 <- getCurrentTime
          putStrLn ("Delete time (N=70): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq e80 (return ())
          t1 <- getCurrentTime
          putStrLn ("Delete time (N=80): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq e90 (return ())
          t1 <- getCurrentTime
          putStrLn ("Delete time (N=90): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq e100 (return ())
          t1 <- getCurrentTime
          putStrLn ("Delete time (N=100): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          -- Lookup
          seq v10 (return ())
          t1 <- getCurrentTime
          putStrLn ("Lookup time (N=10): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq v20 (return ())
          t1 <- getCurrentTime
          putStrLn ("Lookup time (N=20): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq v30 (return ())
          t1 <- getCurrentTime
          putStrLn ("Lookup time (N=30): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq v40 (return ())
          t1 <- getCurrentTime
          putStrLn ("Lookup time (N=40): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq v50 (return ())
          t1 <- getCurrentTime
          putStrLn ("Lookup time (N=50): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq v60 (return ())
          t1 <- getCurrentTime
          putStrLn ("Lookup time (N=60): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq v70 (return ())
          t1 <- getCurrentTime
          putStrLn ("Lookup time (N=70): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq v80 (return ())
          t1 <- getCurrentTime
          putStrLn ("Lookup time (N=80): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq v90 (return ())
          t1 <- getCurrentTime
          putStrLn ("Lookup time (N=90): " ++ show (diffUTCTime t0 t1) ++ " seconds")
          seq v100 (return ())
          t1 <- getCurrentTime
          putStrLn ("Lookup time (N=100): " ++ show (diffUTCTime t0 t1) ++ " seconds")
