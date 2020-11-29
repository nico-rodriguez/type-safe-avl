{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Data.Proxy             (Proxy (Proxy))
import           Data.Tree.AVL.Intern   (lookupAVL)
import           Prelude                (IO, putStrLn, return,
                                        seq, show, (++), ($))
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import AVL.Intern.Insert.Insert10 (t10)
import AVL.Intern.Insert.Insert20 (t20)
import AVL.Intern.Insert.Insert30 (t30)
import AVL.Intern.Insert.Insert40 (t40)
import AVL.Intern.Insert.Insert50 (t50)
import AVL.Intern.Insert.Insert60 (t60)
import AVL.Intern.Delete.Delete10 (e10)
import AVL.Intern.Delete.Delete20 (e20)
import AVL.Intern.Delete.Delete30 (e30)
import AVL.Intern.Delete.Delete40 (e40)
import AVL.Intern.Delete.Delete50 (e50)
import AVL.Intern.Delete.Delete60 (e60)


v10 = lookupAVL (Proxy::Proxy 0) t10
v20 = lookupAVL (Proxy::Proxy 0) t20
v30 = lookupAVL (Proxy::Proxy 0) t30
v40 = lookupAVL (Proxy::Proxy 0) t40
v50 = lookupAVL (Proxy::Proxy 0) t50
v60 = lookupAVL (Proxy::Proxy 0) t60

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
        -- Delete
        putStrLn "DELETE"
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
        -- Lookup
        putStrLn "LOOKUP"
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
