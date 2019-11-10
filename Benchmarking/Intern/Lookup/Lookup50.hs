{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe #-}

import           Prelude (Bool(False), IO, return, seq, (++), putStrLn, show, Char)
import           Intern.AVL (AVL(), lookupAVL)
import           Data.Proxy (Proxy(Proxy))
import           Benchmarking.Intern.Operations (InsertN, insertN)
import           System.Time (getClockTime)
import           Benchmarking.Utils (secDiff)


t50 :: AVL (InsertN 50)
t50 = insertN (Proxy::Proxy 50) (Proxy::Proxy 'False)

v :: Char
v = lookupAVL (Proxy::Proxy 50) t50

main :: IO ()
main = do seq t50 (return ())
          t0 <- getClockTime
          seq v (return ())
          t1 <- getClockTime
          putStrLn ("Time: " ++ show (secDiff t0 t1) ++ " seconds")
