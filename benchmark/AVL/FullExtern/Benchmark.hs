{-# OPTIONS_HADDOCK hide, prune #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Data.Proxy                     (Proxy (Proxy))
import           Data.Tree.AVL.FullExtern       (AVL (AVL))
import           Data.Tree.BST.FullExtern       (lookup)
import           Prelude                        (IO, putStrLn, return,
                                                show, (++), ($), print)
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
    print t10
    print t20
    print t30
    print t40
    print t50
    print t60
    print t70
    print t80
    print t90
    print t100
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
    t0 <- getCurrentTime
    print t70'
    t1 <- getCurrentTime
    putStrLn $ "N=70: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    print t80'
    t1 <- getCurrentTime
    putStrLn $ "N=80: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    print t90'
    t1 <- getCurrentTime
    putStrLn $ "N=90: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    print t100'
    t1 <- getCurrentTime
    putStrLn $ "N=100: " ++ show (diffUTCTime t1 t0)
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
    t0 <- getCurrentTime
    print e70
    t1 <- getCurrentTime
    putStrLn $ "N=70: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    print e80
    t1 <- getCurrentTime
    putStrLn $ "N=80: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    print e90
    t1 <- getCurrentTime
    putStrLn $ "N=90: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    print e100
    t1 <- getCurrentTime
    putStrLn $ "N=100: " ++ show (diffUTCTime t1 t0)
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
    t0 <- getCurrentTime
    print v70
    t1 <- getCurrentTime
    putStrLn $ "N=70: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    print v80
    t1 <- getCurrentTime
    putStrLn $ "N=80: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    print v90
    t1 <- getCurrentTime
    putStrLn $ "N=90: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    print v100
    t1 <- getCurrentTime
    putStrLn $ "N=100: " ++ show (diffUTCTime t1 t0)
