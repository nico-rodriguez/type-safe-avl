{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Prelude              (IO, putStrLn, return,
                                      seq, show, (++), ($))
import           Data.Time.Clock      (diffUTCTime, getCurrentTime)
import AVL.Unsafe.Insert.Insert1 (t1)
import AVL.Unsafe.Insert.Insert2 (t2)
import AVL.Unsafe.Insert.Insert3 (t3)
import AVL.Unsafe.Insert.Insert4 (t4)
import AVL.Unsafe.Insert.Insert5 (t5)
import AVL.Unsafe.Insert.Insert6 (t6)
import AVL.Unsafe.Insert.Insert7 (t7)
import AVL.Unsafe.Insert.Insert8 (t8)
import AVL.Unsafe.Insert.Insert9 (t9)
import AVL.Unsafe.Insert.Insert10 (t10)
import AVL.Unsafe.Lookup.Lookup1 (v1)
import AVL.Unsafe.Lookup.Lookup2 (v2)
import AVL.Unsafe.Lookup.Lookup3 (v3)
import AVL.Unsafe.Lookup.Lookup4 (v4)
import AVL.Unsafe.Lookup.Lookup5 (v5)
import AVL.Unsafe.Lookup.Lookup6 (v6)
import AVL.Unsafe.Lookup.Lookup7 (v7)
import AVL.Unsafe.Lookup.Lookup8 (v8)
import AVL.Unsafe.Lookup.Lookup9 (v9)
import AVL.Unsafe.Lookup.Lookup10 (v10)
import AVL.Unsafe.Delete.Delete1 (e1)
import AVL.Unsafe.Delete.Delete2 (e2)
import AVL.Unsafe.Delete.Delete3 (e3)
import AVL.Unsafe.Delete.Delete4 (e4)
import AVL.Unsafe.Delete.Delete5 (e5)
import AVL.Unsafe.Delete.Delete6 (e6)
import AVL.Unsafe.Delete.Delete7 (e7)
import AVL.Unsafe.Delete.Delete8 (e8)
import AVL.Unsafe.Delete.Delete9 (e9)
import AVL.Unsafe.Delete.Delete10 (e10)


main :: IO ()
main =
    do
        -- Insert
        putStrLn "INSERT"
        t0 <- getCurrentTime
        seq t1 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=10: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq t2 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=20: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq t3 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=30: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq t4 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=40: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq t5 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=50: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq t6 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=60: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq t7 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=70: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq t8 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=80: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq t9 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=90: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq t10 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=100: " ++ show (diffUTCTime t1 t0)
        -- Delete
        putStrLn "DELETE"
        t0 <- getCurrentTime
        seq e1 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=10: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq e2 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=20: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq e3 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=30: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq e4 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=40: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq e5 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=50: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq e6 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=60: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq e7 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=70: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq e8 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=80: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq e9 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=90: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq e10 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=100: " ++ show (diffUTCTime t1 t0)
        -- Lookup
        putStrLn "LOOKUP"
        t0 <- getCurrentTime
        seq v1 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=10: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq v2 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=20: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq v3 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=30: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq v4 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=40: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq v5 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=50: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq v6 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=60: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq v7 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=70: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq v8 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=80: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq v9 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=90: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq v10 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=100: " ++ show (diffUTCTime t1 t0)
