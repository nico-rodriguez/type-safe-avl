{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Data.Tree.AVL.Unsafe (lookupAVL)
import           Prelude              (IO, putStrLn, return,
                                      seq, show, (++), ($))
import           Data.Time.Clock      (diffUTCTime, getCurrentTime)
import AVL.Unsafe.Insert.Insert10 (t10)
import AVL.Unsafe.Insert.Insert20 (t20)
import AVL.Unsafe.Insert.Insert30 (t30)
import AVL.Unsafe.Insert.Insert40 (t40)
import AVL.Unsafe.Insert.Insert50 (t50)
import AVL.Unsafe.Insert.Insert60 (t60)
import AVL.Unsafe.Insert.Insert70 (t70)
import AVL.Unsafe.Insert.Insert80 (t80)
import AVL.Unsafe.Insert.Insert90 (t90)
import AVL.Unsafe.Insert.Insert100 (t100)
import AVL.Unsafe.Delete.Delete10 (e10)
import AVL.Unsafe.Delete.Delete20 (e20)
import AVL.Unsafe.Delete.Delete30 (e30)
import AVL.Unsafe.Delete.Delete40 (e40)
import AVL.Unsafe.Delete.Delete50 (e50)
import AVL.Unsafe.Delete.Delete60 (e60)
import AVL.Unsafe.Delete.Delete70 (e70)
import AVL.Unsafe.Delete.Delete80 (e80)
import AVL.Unsafe.Delete.Delete90 (e90)
import AVL.Unsafe.Delete.Delete100 (e100)


v10 = lookupAVL 9 t10
v20 = lookupAVL 19 t20
v30 = lookupAVL 29 t30
v40 = lookupAVL 39 t40
v50 = lookupAVL 49 t50
v60 = lookupAVL 59 t60
v70 = lookupAVL 69 t70
v80 = lookupAVL 79 t80
v90 = lookupAVL 89 t90
v100 = lookupAVL 99 t100

main :: IO ()
main =
    do
        -- Insert
        putStrLn "INSERT"
        t0 <- getCurrentTime
        seq t10 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=10: " ++ show (diffUTCTime t0 t1)
        seq t20 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=20: " ++ show (diffUTCTime t0 t1)
        seq t30 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=30: " ++ show (diffUTCTime t0 t1)
        seq t40 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=40: " ++ show (diffUTCTime t0 t1)
        seq t50 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=50: " ++ show (diffUTCTime t0 t1)
        seq t60 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=60: " ++ show (diffUTCTime t0 t1)
        seq t70 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=70: " ++ show (diffUTCTime t0 t1)
        seq t80 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=80: " ++ show (diffUTCTime t0 t1)
        seq t90 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=90: " ++ show (diffUTCTime t0 t1)
        seq t100 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=100: " ++ show (diffUTCTime t0 t1)
        -- Delete
        seq e10 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=10: " ++ show (diffUTCTime t0 t1)
        seq e20 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=20: " ++ show (diffUTCTime t0 t1)
        seq e30 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=30: " ++ show (diffUTCTime t0 t1)
        seq e40 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=40: " ++ show (diffUTCTime t0 t1)
        seq e50 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=50: " ++ show (diffUTCTime t0 t1)
        seq e60 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=60: " ++ show (diffUTCTime t0 t1)
        seq e70 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=70: " ++ show (diffUTCTime t0 t1)
        seq e80 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=80: " ++ show (diffUTCTime t0 t1)
        seq e90 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=90: " ++ show (diffUTCTime t0 t1)
        seq e100 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=100: " ++ show (diffUTCTime t0 t1)
        -- Lookup
        seq v10 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=10: " ++ show (diffUTCTime t0 t1)
        seq v20 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=20: " ++ show (diffUTCTime t0 t1)
        seq v30 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=30: " ++ show (diffUTCTime t0 t1)
        seq v40 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=40: " ++ show (diffUTCTime t0 t1)
        seq v50 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=50: " ++ show (diffUTCTime t0 t1)
        seq v60 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=60: " ++ show (diffUTCTime t0 t1)
        seq v70 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=70: " ++ show (diffUTCTime t0 t1)
        seq v80 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=80: " ++ show (diffUTCTime t0 t1)
        seq v90 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=90: " ++ show (diffUTCTime t0 t1)
        seq v100 (return ())
        t1 <- getCurrentTime
        putStrLn $ "N=100: " ++ show (diffUTCTime t0 t1)
