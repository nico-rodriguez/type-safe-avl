{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Prelude              (IO, putStrLn, return,
                                      show, (++), ($))
import           Data.Time.Clock      (diffUTCTime, getCurrentTime)
import           Control.Deepseq      (deepseq)
import AVL.Unsafe.Example.Example1 (t1)
import AVL.Unsafe.Example.Example2 (t2)
import AVL.Unsafe.Example.Example3 (t3)
import AVL.Unsafe.Example.Example4 (t4)
import AVL.Unsafe.Example.Example5 (t5)
import AVL.Unsafe.Example.Example6 (t6)
import AVL.Unsafe.Example.Example7 (t7)
import AVL.Unsafe.Example.Example8 (t8)
import AVL.Unsafe.Example.Example9 (t9)
import AVL.Unsafe.Example.Example10 (t10)
import AVL.Unsafe.Insert.Insert1 (t1')
import AVL.Unsafe.Insert.Insert2 (t2')
import AVL.Unsafe.Insert.Insert3 (t3')
import AVL.Unsafe.Insert.Insert4 (t4')
import AVL.Unsafe.Insert.Insert5 (t5')
import AVL.Unsafe.Insert.Insert6 (t6')
import AVL.Unsafe.Insert.Insert7 (t7')
import AVL.Unsafe.Insert.Insert8 (t8')
import AVL.Unsafe.Insert.Insert9 (t9')
import AVL.Unsafe.Insert.Insert10 (t10')
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
        -- Pre evaluate the example trees
        deepseq t1 (return ())
        deepseq t2 (return ())
        deepseq t3 (return ())
        deepseq t4 (return ())
        deepseq t5 (return ())
        deepseq t6 (return ())
        deepseq t7 (return ())
        deepseq t8 (return ())
        deepseq t9 (return ())
        deepseq t10 (return ())
        -- Insert
        putStrLn "INSERT"
        t0 <- getCurrentTime
        deepseq t1' (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=2^6: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        deepseq t2' (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=2^7: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        deepseq t3' (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=2^8: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        deepseq t4' (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=2^9: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        deepseq t5' (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=2^10: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        deepseq t6' (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=2^11: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        deepseq t7' (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=2^12: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        deepseq t8' (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=2^13: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        deepseq t9' (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=2^14: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        deepseq t10' (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=2^15: " ++ show (diffUTCTime t1 t0)
        -- Delete
        putStrLn "DELETE"
        t0 <- getCurrentTime
        deepseq e1 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=2^6: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        deepseq e2 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=2^7: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        deepseq e3 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=2^8: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        deepseq e4 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=2^9: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        deepseq e5 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=2^10: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        deepseq e6 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=2^11: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        deepseq e7 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=2^12: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        deepseq e8 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=2^13: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        deepseq e9 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=2^14: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        deepseq e10 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=2^15: " ++ show (diffUTCTime t1 t0)
        -- Lookup
        putStrLn "LOOKUP"
        t0 <- getCurrentTime
        deepseq v1 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=2^6: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        deepseq v2 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=2^7: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        deepseq v3 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=2^8: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        deepseq v4 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=2^9: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        deepseq v5 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=2^10: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        deepseq v6 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=2^11: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        deepseq v7 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=2^12: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        deepseq v8 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=2^13: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        deepseq v9 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=2^14: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        deepseq v10 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=2^15: " ++ show (diffUTCTime t1 t0)
