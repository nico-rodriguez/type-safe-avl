{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe #-}

import           Prelude (Bool(False), IO, return, seq, (++), putStrLn, show)
import           Intern.AVL (AVL())
import           Data.Proxy (Proxy(Proxy))
import           Benchmarking.Intern.Operations (InsertN, insertN)
import           System.Time (getClockTime)
import           Benchmarking.Utils (secDiff)


t50 :: AVL (InsertN 50)
t50 = insertN (Proxy::Proxy 50) (Proxy::Proxy 'False)

main :: IO ()
main = do t0 <- getClockTime
          seq t50 (return ())
          t1 <- getClockTime
          putStrLn ("Time: " ++ show (secDiff t0 t1) ++ " seconds")
