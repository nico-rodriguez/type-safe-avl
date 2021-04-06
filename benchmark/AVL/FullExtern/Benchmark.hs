{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Data.Proxy                     (Proxy (Proxy))
import           Data.Tree.AVL.FullExtern       (AVL (AVL))
import           Data.Tree.BST.FullExtern       (lookup)
import           Prelude                        (IO, putStrLn, return,
                                                seq, show, (++), ($))
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import AVL.FullExtern.Example.Example10 (t10)
import AVL.FullExtern.Example.Example20 (t20)
import AVL.FullExtern.Example.Example30 (t30)
import AVL.FullExtern.Example.Example40 (t40)
import AVL.FullExtern.Example.Example50 (t50)
import AVL.FullExtern.Example.Example60 (t60)
import AVL.FullExtern.Example.Example70 (t70)
import AVL.FullExtern.Example.Example80 (t80)
import AVL.FullExtern.Example.Example90 (t90)
import AVL.FullExtern.Example.Example100 (t100)
import AVL.FullExtern.Insert.Insert10 (t10')
import AVL.FullExtern.Insert.Insert20 (t20')
import AVL.FullExtern.Insert.Insert30 (t30')
import AVL.FullExtern.Insert.Insert40 (t40')
import AVL.FullExtern.Insert.Insert50 (t50')
import AVL.FullExtern.Insert.Insert60 (t60')
import AVL.FullExtern.Insert.Insert70 (t70')
import AVL.FullExtern.Insert.Insert80 (t80')
import AVL.FullExtern.Insert.Insert90 (t90')
import AVL.FullExtern.Insert.Insert100 (t100')
import AVL.FullExtern.Lookup.Lookup10 (v10)
import AVL.FullExtern.Lookup.Lookup20 (v20)
import AVL.FullExtern.Lookup.Lookup30 (v30)
import AVL.FullExtern.Lookup.Lookup40 (v40)
import AVL.FullExtern.Lookup.Lookup50 (v50)
import AVL.FullExtern.Lookup.Lookup60 (v60)
import AVL.FullExtern.Lookup.Lookup70 (v70)
import AVL.FullExtern.Lookup.Lookup80 (v80)
import AVL.FullExtern.Lookup.Lookup90 (v90)
import AVL.FullExtern.Lookup.Lookup100 (v100)
import AVL.FullExtern.Delete.Delete10 (e10)
import AVL.FullExtern.Delete.Delete20 (e20)
import AVL.FullExtern.Delete.Delete30 (e30)
import AVL.FullExtern.Delete.Delete40 (e40)
import AVL.FullExtern.Delete.Delete50 (e50)
import AVL.FullExtern.Delete.Delete60 (e60)
import AVL.FullExtern.Delete.Delete70 (e70)
import AVL.FullExtern.Delete.Delete80 (e80)
import AVL.FullExtern.Delete.Delete90 (e90)
import AVL.FullExtern.Delete.Delete100 (e100)


main :: IO ()
main =
  do
    -- Pre evaluate the example trees
    seq t10 (return ())
    seq t20 (return ())
    seq t30 (return ())
    seq t40 (return ())
    seq t50 (return ())
    seq t60 (return ())
    seq t70 (return ())
    seq t80 (return ())
    seq t90 (return ())
    seq t100 (return ())
    -- Insert
    putStrLn "INSERT"
    t0 <- getCurrentTime
    seq t10' (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=10: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    seq t20' (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=20: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    seq t30' (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=30: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    seq t40' (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=40: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    seq t50' (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=50: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    seq t60' (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=60: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    seq t70' (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=70: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    seq t80' (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=80: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    seq t90' (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=90: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    seq t100' (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=100: " ++ show (diffUTCTime t1 t0)
    -- Delete
    putStrLn "DELETE"
    t0 <- getCurrentTime
    seq e10 (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=10: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    seq e20 (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=20: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    seq e30 (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=30: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    seq e40 (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=40: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    seq e50 (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=50: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    seq e60 (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=60: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    seq e70 (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=70: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    seq e80 (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=80: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    seq e90 (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=90: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    seq e100 (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=100: " ++ show (diffUTCTime t1 t0)
    -- Lookup
    putStrLn "LOOKUP"
    t0 <- getCurrentTime
    seq v10 (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=10: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    seq v20 (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=20: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    seq v30 (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=30: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    seq v40 (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=40: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    seq v50 (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=50: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    seq v60 (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=60: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    seq v70 (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=70: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    seq v80 (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=80: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    seq v90 (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=90: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    seq v100 (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=100: " ++ show (diffUTCTime t1 t0)
