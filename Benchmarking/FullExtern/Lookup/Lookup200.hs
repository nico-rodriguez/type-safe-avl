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


t200 :: AVL (InsertN 200)
t200 = insertN (Proxy::Proxy 200) (Proxy::Proxy 'False)

v :: Char
v = lookupAVL (Proxy::Proxy 200) t200

main :: IO ()
main = do seq t200 (return ())
          t0 <- getClockTime
          seq v (return ())
          t1 <- getClockTime
          putStrLn ("Time: " ++ show (secDiff t0 t1) ++ " seconds")
