{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Data.Proxy                     (Proxy (Proxy))
import           Data.Tree.BST.FullExtern       (BST (BST), lookup)
import           Prelude                        (IO, putStrLn, return,
                                                seq, show, (++), ($))
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import BST.FullExtern.Insert.Insert10 (t10)
import BST.FullExtern.Insert.Insert20 (t20)
import BST.FullExtern.Insert.Insert30 (t30)
import BST.FullExtern.Insert.Insert40 (t40)
import BST.FullExtern.Insert.Insert50 (t50)
import BST.FullExtern.Insert.Insert60 (t60)
import BST.FullExtern.Delete.Delete10 (e10)
import BST.FullExtern.Delete.Delete20 (e20)
import BST.FullExtern.Delete.Delete30 (e30)
import BST.FullExtern.Delete.Delete40 (e40)
import BST.FullExtern.Delete.Delete50 (e50)
import BST.FullExtern.Delete.Delete60 (e60)


v10 = case t10 of
  BST t10' _ -> lookup (Proxy::Proxy 0) t10'
v20 = case t20 of
  BST t20' _ -> lookup (Proxy::Proxy 0) t20'
v30 = case t30 of
  BST t30' _ -> lookup (Proxy::Proxy 0) t30'
v40 = case t40 of
  BST t40' _ -> lookup (Proxy::Proxy 0) t40'
v50 = case t50 of
  BST t50' _ -> lookup (Proxy::Proxy 0) t50'
v60 = case t60 of
  BST t60' _ -> lookup (Proxy::Proxy 0) t60'

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
