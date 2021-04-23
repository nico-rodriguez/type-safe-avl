{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Prelude              (IO, putStrLn, return,
                                      show, (++), ($), print)
import           Data.Time.Clock      (diffUTCTime, getCurrentTime)
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
        print t1
        print t2
        print t3
        print t4
        print t5
        print t6
        print t7
        print t8
        print t9
        print t10
        -- Insert
        putStrLn "INSERT"
        t0 <- getCurrentTime
        print t1'
        t1 <- getCurrentTime
        putStrLn $ "N=4^1: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        print t2'
        t1 <- getCurrentTime
        putStrLn $ "N=4^2: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        print t3'
        t1 <- getCurrentTime
        putStrLn $ "N=4^3: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        print t4'
        t1 <- getCurrentTime
        putStrLn $ "N=4^4: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        print t5'
        t1 <- getCurrentTime
        putStrLn $ "N=4^5: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        print t6'
        t1 <- getCurrentTime
        putStrLn $ "N=4^6: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        print t7'
        t1 <- getCurrentTime
        putStrLn $ "N=4^7: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        print t8'
        t1 <- getCurrentTime
        putStrLn $ "N=4^8: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        print t9'
        t1 <- getCurrentTime
        putStrLn $ "N=4^9: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        print t10'
        t1 <- getCurrentTime
        putStrLn $ "N=4^10: " ++ show (diffUTCTime t1 t0)
        -- Delete
        putStrLn "DELETE"
        t0 <- getCurrentTime
        print e1
        t1 <- getCurrentTime
        putStrLn $ "N=4^1: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        print e2
        t1 <- getCurrentTime
        putStrLn $ "N=4^2: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        print e3
        t1 <- getCurrentTime
        putStrLn $ "N=4^3: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        print e4
        t1 <- getCurrentTime
        putStrLn $ "N=4^4: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        print e5
        t1 <- getCurrentTime
        putStrLn $ "N=4^5: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        print e6
        t1 <- getCurrentTime
        putStrLn $ "N=4^6: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        print e7
        t1 <- getCurrentTime
        putStrLn $ "N=4^7: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        print e8
        t1 <- getCurrentTime
        putStrLn $ "N=4^8: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        print e9
        t1 <- getCurrentTime
        putStrLn $ "N=4^9: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        print e10
        t1 <- getCurrentTime
        putStrLn $ "N=4^10: " ++ show (diffUTCTime t1 t0)
        -- Lookup
        putStrLn "LOOKUP"
        t0 <- getCurrentTime
        print v1
        t1 <- getCurrentTime
        putStrLn $ "N=4^1: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        print v2
        t1 <- getCurrentTime
        putStrLn $ "N=4^2: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        print v3
        t1 <- getCurrentTime
        putStrLn $ "N=4^3: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        print v4
        t1 <- getCurrentTime
        putStrLn $ "N=4^4: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        print v5
        t1 <- getCurrentTime
        putStrLn $ "N=4^5: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        print v6
        t1 <- getCurrentTime
        putStrLn $ "N=4^6: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        print v7
        t1 <- getCurrentTime
        putStrLn $ "N=4^7: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        print v8
        t1 <- getCurrentTime
        putStrLn $ "N=4^8: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        print v9
        t1 <- getCurrentTime
        putStrLn $ "N=4^9: " ++ show (diffUTCTime t1 t0)
        t0 <- getCurrentTime
        print v10
        t1 <- getCurrentTime
        putStrLn $ "N=4^10: " ++ show (diffUTCTime t1 t0)
