{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe #-}

import           Prelude (Bool(False), IO, return, seq, (++), putStrLn, show, Char)
import           Intern.AVL (AVL(), lookupAVL)
import           Data.Proxy (Proxy(Proxy))
import           Benchmarking.Intern.Operations (InsertN, insertN)
import           System.Time (getClockTime)
import           Benchmarking.Utils (secDiff)


t40 :: AVL (InsertN 40)
t40 = insertN (Proxy::Proxy 40) (Proxy::Proxy 'False)

v :: Char
v = lookupAVL (Proxy::Proxy 40) t40

main :: IO ()
main = do seq t40 (return ())
          t0 <- getClockTime
          seq v (return ())
          t1 <- getClockTime
          putStrLn ("Time: " ++ show (secDiff t0 t1) ++ " seconds")
