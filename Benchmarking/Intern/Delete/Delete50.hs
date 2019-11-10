{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe #-}

import           ITree (Tree(EmptyTree))
import           Prelude (Bool(False), IO, return, seq, (++), putStrLn, show)
import           Intern.AVL (AVL())
import           Data.Proxy (Proxy(Proxy))
import           Benchmarking.Intern.Operations (InsertN, insertN, deleteN)
import           System.Time (getClockTime)
import           Benchmarking.Utils (secDiff)


t50 :: AVL (InsertN 50)
t50 = insertN (Proxy::Proxy 50) (Proxy::Proxy 'False)
e :: AVL 'EmptyTree
e = deleteN (Proxy::Proxy 50) t50

main :: IO ()
main = do seq t50 (return ())
          t0 <- getClockTime
          seq e (return ())
          t1 <- getClockTime
          putStrLn ("Time: " ++ show (secDiff t0 t1) ++ " seconds")
