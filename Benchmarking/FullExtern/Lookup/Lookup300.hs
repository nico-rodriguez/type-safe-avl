{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Benchmarking.FullExtern.Operations (InsertN, insertN)
import           Benchmarking.Utils                 (secDiff)
import           Data.Proxy                         (Proxy (Proxy))
import           Extern.AVL                         (AVL (), lookupAVL)
import           Prelude                            (Bool (False), Char, IO,
                                                     putStrLn, return, seq,
                                                     show, (++))
import           System.Time                        (getClockTime)


t300 :: AVL (InsertN 300)
t300 = insertN (Proxy::Proxy 300) (Proxy::Proxy 'False)

v :: Char
v = lookupAVL (Proxy::Proxy 300) t300

main :: IO ()
main = do seq t300 (return ())
          t0 <- getClockTime
          seq v (return ())
          t1 <- getClockTime
          putStrLn ("Time: " ++ show (secDiff t0 t1) ++ " seconds")
