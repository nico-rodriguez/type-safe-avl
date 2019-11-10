{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe #-}

import           Prelude (Bool(False), IO, return, seq, (++), putStrLn, show)
import           Extern.AVL (AVL())
import           Data.Proxy (Proxy(Proxy))
import           Benchmarking.FullExtern.Operations (InsertN, insertN)
import           System.Time (getClockTime)
import           Benchmarking.Utils (secDiff)


t30 :: AVL (InsertN 30)
t30 = insertN (Proxy::Proxy 30) (Proxy::Proxy 'False)

main :: IO ()
main = do t0 <- getClockTime
          seq t30 (return ())
          t1 <- getClockTime
          putStrLn ("Time: " ++ show (secDiff t0 t1) ++ " seconds")
