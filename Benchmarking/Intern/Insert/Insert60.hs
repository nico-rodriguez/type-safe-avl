{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe #-}

import           Prelude (Bool(False), IO, return, seq, (++), putStrLn, show)
import           Intern.AVL (AVL())
import           Data.Proxy (Proxy(Proxy))
import           Benchmarking.Intern.Operations (InsertN, insertN)
import           System.Time (getClockTime)
import           Benchmarking.Utils (secDiff)


t60 :: AVL (InsertN 60)
t60 = insertN (Proxy::Proxy 60) (Proxy::Proxy 'False)

main :: IO ()
main = do t0 <- getClockTime
          seq t60 (return ())
          t1 <- getClockTime
          putStrLn ("Time: " ++ show (secDiff t0 t1) ++ " seconds")
