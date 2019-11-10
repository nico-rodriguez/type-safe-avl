{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe #-}

import           Prelude (Bool(False), IO, return, seq, (++), putStrLn, show)
import           Extern.AVL (AVL())
import           Data.Proxy (Proxy(Proxy))
import           Benchmarking.Extern.Operations (InsertN, insertN)
import           System.Time (getClockTime)
import           Benchmarking.Utils (secDiff)


t10 :: AVL (InsertN 10)
t10 = insertN (Proxy::Proxy 10) (Proxy::Proxy 'False)

main :: IO ()
main = do t0 <- getClockTime
          seq t10 (return ())
          t1 <- getClockTime
          putStrLn ("Time: " ++ show (secDiff t0 t1) ++ " seconds")
