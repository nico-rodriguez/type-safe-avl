{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe #-}

import           ITree (Tree(EmptyTree))
import           Prelude (Bool(False), IO, return, seq, (++), putStrLn, show)
import           Extern.AVL (AVL())
import           Data.Proxy (Proxy(Proxy))
import           Benchmarking.FullExtern.Operations (InsertN, insertN, deleteN)
import           System.Time (getClockTime)
import           Benchmarking.Utils (secDiff)


t30 :: AVL (InsertN 30)
t30 = insertN (Proxy::Proxy 30) (Proxy::Proxy 'False)
e :: AVL 'EmptyTree
e = deleteN (Proxy::Proxy 30) t30

main :: IO ()
main = do seq t30 (return ())
          t0 <- getClockTime
          seq e (return ())
          t1 <- getClockTime
          putStrLn ("Time: " ++ show (secDiff t0 t1) ++ " seconds")
