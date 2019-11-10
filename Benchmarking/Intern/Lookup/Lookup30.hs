{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe #-}

import           Prelude (Bool(False), IO, return, seq, (++), putStrLn, show, Char)
import           Intern.AVL (AVL(), lookupAVL)
import           Data.Proxy (Proxy(Proxy))
import           Benchmarking.Intern.Operations (InsertN, insertN)
import           System.Time (getClockTime)
import           Benchmarking.Utils (secDiff)


t30 :: AVL (InsertN 30)
t30 = insertN (Proxy::Proxy 30) (Proxy::Proxy 'False)

v :: Char
v = lookupAVL (Proxy::Proxy 30) t30

main :: IO ()
main = do seq t30 (return ())
          t0 <- getClockTime
          seq v (return ())
          t1 <- getClockTime
          putStrLn ("Time: " ++ show (secDiff t0 t1) ++ " seconds")
