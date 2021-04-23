{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Prelude              (IO, putStrLn, return,
                                      show, (++), ($), seq)
import           Data.Time.Clock      (diffUTCTime, getCurrentTime)
import BST.Unsafe.Example.Example1 (t1)
import BST.Unsafe.Example.Example2 (t2)
import BST.Unsafe.Example.Example3 (t3)
import BST.Unsafe.Example.Example4 (t4)
import BST.Unsafe.Example.Example5 (t5)
import BST.Unsafe.Example.Example6 (t6)
import BST.Unsafe.Example.Example7 (t7)
import BST.Unsafe.Example.Example8 (t8)
import BST.Unsafe.Example.Example9 (t9)
import BST.Unsafe.Example.Example10 (t10)
import BST.Unsafe.Insert.Insert1 (t1')
import BST.Unsafe.Insert.Insert2 (t2')
import BST.Unsafe.Insert.Insert3 (t3')
import BST.Unsafe.Insert.Insert4 (t4')
import BST.Unsafe.Insert.Insert5 (t5')
import BST.Unsafe.Insert.Insert6 (t6')
import BST.Unsafe.Insert.Insert7 (t7')
import BST.Unsafe.Insert.Insert8 (t8')
import BST.Unsafe.Insert.Insert9 (t9')
import BST.Unsafe.Insert.Insert10 (t10')
import BST.Unsafe.Lookup.Lookup1 (v1)
import BST.Unsafe.Lookup.Lookup2 (v2)
import BST.Unsafe.Lookup.Lookup3 (v3)
import BST.Unsafe.Lookup.Lookup4 (v4)
import BST.Unsafe.Lookup.Lookup5 (v5)
import BST.Unsafe.Lookup.Lookup6 (v6)
import BST.Unsafe.Lookup.Lookup7 (v7)
import BST.Unsafe.Lookup.Lookup8 (v8)
import BST.Unsafe.Lookup.Lookup9 (v9)
import BST.Unsafe.Lookup.Lookup10 (v10)
import BST.Unsafe.Delete.Delete1 (e1)
import BST.Unsafe.Delete.Delete2 (e2)
import BST.Unsafe.Delete.Delete3 (e3)
import BST.Unsafe.Delete.Delete4 (e4)
import BST.Unsafe.Delete.Delete5 (e5)
import BST.Unsafe.Delete.Delete6 (e6)
import BST.Unsafe.Delete.Delete7 (e7)
import BST.Unsafe.Delete.Delete8 (e8)
import BST.Unsafe.Delete.Delete9 (e9)
import BST.Unsafe.Delete.Delete10 (e10)


main :: IO ()
main =
    do
        -- Pre evaluate the example trees
        seq t1 (return ())
        seq t2 (return ())
        seq t3 (return ())
        seq t4 (return ())
        seq t5 (return ())
        seq t6 (return ())
        seq t7 (return ())
        seq t8 (return ())
        seq t9 (return ())
        seq t10 (return ())
        -- Insert
        putStrLn "INSERT"
        t0 <- getCurrentTime
        seq t1' (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=4^1: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq t2' (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=4^2: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq t3' (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=4^3: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq t4' (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=4^4: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq t5' (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=4^5: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq t6' (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=4^6: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq t7' (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=4^7: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq t8' (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=4^8: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq t9' (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=4^9: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq t10' (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=4^10: " ++ show (diffUTCTime t1 t0)
        -- Delete
        putStrLn "DELETE"
        t0 <- getCurrentTime
        seq e1 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=4^1: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq e2 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=4^2: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq e3 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=4^3: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq e4 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=4^4: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq e5 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=4^5: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq e6 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=4^6: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq e7 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=4^7: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq e8 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=4^8: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq e9 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=4^9: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq e10 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=4^10: " ++ show (diffUTCTime t1 t0)
        -- Lookup
        putStrLn "LOOKUP"
        t0 <- getCurrentTime
        seq v1 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=4^1: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq v2 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=4^2: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq v3 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=4^3: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq v4 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=4^4: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq v5 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=4^5: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq v6 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=4^6: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq v7 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=4^7: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq v8 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=4^8: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq v9 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=4^9: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        seq v10 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=4^10: " ++ show (diffUTCTime t1 t0)
