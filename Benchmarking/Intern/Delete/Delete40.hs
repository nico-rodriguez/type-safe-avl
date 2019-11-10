{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe #-}

import           ITree (Tree(EmptyTree))
import           Prelude (Bool(False), IO, return, seq, (++), putStrLn, show)
import           Intern.AVL (AVL())
import           Data.Proxy (Proxy(Proxy))
import           Benchmarking.Intern.Operations (InsertN, insertN, deleteN)
import           System.Time (getClockTime)
import           Benchmarking.Utils (secDiff)


t40 :: AVL (InsertN 40)
t40 = insertN (Proxy::Proxy 40) (Proxy::Proxy 'False)
e :: AVL 'EmptyTree
e = deleteN (Proxy::Proxy 40) t40

main :: IO ()
main = do seq t40 (return ())
          t0 <- getClockTime
          seq e (return ())
          t1 <- getClockTime
          putStrLn ("Time: " ++ show (secDiff t0 t1) ++ " seconds")
