{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Prelude                (IO, putStrLn, return,
                                        seq, show, (++), ($))
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import AVL.Extern.Insert.Insert10 (t10)
import AVL.Extern.Insert.Insert20 (t20)
import AVL.Extern.Insert.Insert30 (t30)
import AVL.Extern.Insert.Insert40 (t40)
import AVL.Extern.Insert.Insert50 (t50)
import AVL.Extern.Insert.Insert60 (t60)
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
    -- Insert
    putStrLn "INSERT"
    t0 <- getCurrentTime
    seq t10 (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=10: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    seq t20 (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=20: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    seq t30 (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=30: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    seq t40 (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=40: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    seq t50 (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=50: " ++ show (diffUTCTime t1 t0)
    t0 <- getCurrentTime
    seq t60 (return ())
    t1 <- getCurrentTime
    putStrLn $ "N=60: " ++ show (diffUTCTime t1 t0)
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