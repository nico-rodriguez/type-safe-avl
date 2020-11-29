{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Data.Proxy             (Proxy (Proxy))
import           Data.Tree.BST.Extern   (lookupBST)
import           Prelude                (IO, putStrLn, return,
                                        seq, show, (++), ($))
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import BST.Extern.Insert.Insert10 (t10)
import BST.Extern.Insert.Insert20 (t20)
import BST.Extern.Insert.Insert30 (t30)
import BST.Extern.Insert.Insert40 (t40)
import BST.Extern.Insert.Insert50 (t50)
import BST.Extern.Delete.Delete10 (e10)
import BST.Extern.Delete.Delete20 (e20)
import BST.Extern.Delete.Delete30 (e30)
import BST.Extern.Delete.Delete40 (e40)
import BST.Extern.Delete.Delete50 (e50)


v10 = lookupBST (Proxy::Proxy 0) t10
v20 = lookupBST (Proxy::Proxy 0) t20
v30 = lookupBST (Proxy::Proxy 0) t30
v40 = lookupBST (Proxy::Proxy 0) t40
v50 = lookupBST (Proxy::Proxy 0) t50

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
