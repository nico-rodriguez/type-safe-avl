{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Prelude                (IO, putStrLn, return,
                                        show, (++), ($), print)
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import BST.Intern.Example.Example10 (t10)
import BST.Intern.Example.Example20 (t20)
import BST.Intern.Example.Example30 (t30)
import BST.Intern.Example.Example40 (t40)
import BST.Intern.Example.Example50 (t50)
import BST.Intern.Insert.Insert10 (t10')
import BST.Intern.Insert.Insert20 (t20')
import BST.Intern.Insert.Insert30 (t30')
import BST.Intern.Insert.Insert40 (t40')
import BST.Intern.Insert.Insert50 (t50')
import BST.Intern.Lookup.Lookup10 (v10)
import BST.Intern.Lookup.Lookup20 (v20)
import BST.Intern.Lookup.Lookup30 (v30)
import BST.Intern.Lookup.Lookup40 (v40)
import BST.Intern.Lookup.Lookup50 (v50)
import BST.Intern.Delete.Delete10 (e10)
import BST.Intern.Delete.Delete20 (e20)
import BST.Intern.Delete.Delete30 (e30)
import BST.Intern.Delete.Delete40 (e40)
import BST.Intern.Delete.Delete50 (e50)


main :: IO ()
main =
    do 
    -- Pre evaluate the example trees
    print t10
    print t20
    print t30
    print t40
    print t50
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
