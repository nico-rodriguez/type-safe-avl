{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe #-}

import           Prelude (Bool(False), IO, return, seq, (++), putStrLn, show, Char)
import           Extern.AVL (AVL(), lookupAVL)
import           Data.Proxy (Proxy(Proxy))
import           Benchmarking.Extern.Operations (InsertN, insertN)
import           System.Time (getClockTime)
import           Benchmarking.Utils (secDiff)


t10 :: AVL (InsertN 10)
t10 = insertN (Proxy::Proxy 10) (Proxy::Proxy 'False)

v :: Char
v = lookupAVL (Proxy::Proxy 10) t10

main :: IO ()
main = do seq t10 (return ())
          t0 <- getClockTime
          seq v (return ())
          t1 <- getClockTime
          putStrLn ("Time: " ++ show (secDiff t0 t1) ++ " seconds")
