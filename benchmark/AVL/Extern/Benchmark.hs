{-# OPTIONS_HADDOCK hide, prune #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Prelude                (IO, putStrLn, return,
                                        show, (++), ($), print)
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import AVL.Extern.Example.Example10 (t10)
import AVL.Extern.Example.Example20 (t20)
import AVL.Extern.Example.Example30 (t30)
import AVL.Extern.Example.Example40 (t40)
import AVL.Extern.Example.Example50 (t50)
import AVL.Extern.Example.Example60 (t60)
import AVL.Extern.Insert.Insert10 (t10')
import AVL.Extern.Insert.Insert20 (t20')
import AVL.Extern.Insert.Insert30 (t30')
import AVL.Extern.Insert.Insert40 (t40')
import AVL.Extern.Insert.Insert50 (t50')
import AVL.Extern.Insert.Insert60 (t60')
import AVL.Extern.Lookup.Lookup10 (v10)
import AVL.Extern.Lookup.Lookup20 (v20)
import AVL.Extern.Lookup.Lookup30 (v30)
import AVL.Extern.Lookup.Lookup40 (v40)
import AVL.Extern.Lookup.Lookup50 (v50)
import AVL.Extern.Lookup.Lookup60 (v60)
import AVL.Extern.Delete.Delete10 (e10)
import AVL.Extern.Delete.Delete20 (e20)
import AVL.Extern.Delete.Delete30 (e30)
import AVL.Extern.Delete.Delete40 (e40)
import AVL.Extern.Delete.Delete50 (e50)
import AVL.Extern.Delete.Delete60 (e60)


main :: IO ()
main =
    do 
    -- Pre evaluate the example trees
    print t10
    print t20
    print t30
    print t40
    print t50
    print t60
    -- Insert
    putStrLn "INSERT"
    t0 <- getCurrentTime
    print t10'
    t1 <- getCurrentTime
    putStrLn $ "N=10: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    print t20'
    t1 <- getCurrentTime
    putStrLn $ "N=20: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    print t30'
    t1 <- getCurrentTime
    putStrLn $ "N=30: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    print t40'
    t1 <- getCurrentTime
    putStrLn $ "N=40: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    print t50'
    t1 <- getCurrentTime
    putStrLn $ "N=50: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    print t60'
    t1 <- getCurrentTime
    putStrLn $ "N=60: " ++ show (diffUTCTime t1 t0)
    -- Delete
    putStrLn "DELETE"
    t0 <- getCurrentTime
    print e10
    t1 <- getCurrentTime
    putStrLn $ "N=10: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    print e20
    t1 <- getCurrentTime
    putStrLn $ "N=20: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    print e30
    t1 <- getCurrentTime
    putStrLn $ "N=30: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    print e40
    t1 <- getCurrentTime
    putStrLn $ "N=40: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    print e50
    t1 <- getCurrentTime
    putStrLn $ "N=50: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    print e60
    t1 <- getCurrentTime
    putStrLn $ "N=60: " ++ show (diffUTCTime t1 t0)
    -- Lookup
    putStrLn "LOOKUP"
    t0 <- getCurrentTime
    print v10
    t1 <- getCurrentTime
    putStrLn $ "N=10: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    print v20
    t1 <- getCurrentTime
    putStrLn $ "N=20: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    print v30
    t1 <- getCurrentTime
    putStrLn $ "N=30: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    print v40
    t1 <- getCurrentTime
    putStrLn $ "N=40: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    print v50
    t1 <- getCurrentTime
    putStrLn $ "N=50: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    print v60
    t1 <- getCurrentTime
    putStrLn $ "N=60: " ++ show (diffUTCTime t1 t0)
