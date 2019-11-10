{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe #-}

import           Prelude (Bool(False), IO, return, seq, (++), putStrLn, show)
import           Intern.AVL (AVL())
import           Data.Proxy (Proxy(Proxy))
import           Benchmarking.Intern.Operations (InsertN, insertN)
import           System.Time (getClockTime)
import           Benchmarking.Utils (secDiff)


t20 :: AVL (InsertN 20)
t20 = insertN (Proxy::Proxy 20) (Proxy::Proxy 'False)

main :: IO ()
main = do t0 <- getClockTime
          seq t20 (return ())
          t1 <- getClockTime
          putStrLn ("Time: " ++ show (secDiff t0 t1) ++ " seconds")
