{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Benchmarking.FullExtern.Operations (InsertN, insertN)
import           Benchmarking.Utils                 (secDiff)
import           Data.Proxy                         (Proxy (Proxy))
import           Extern.AVL                         (AVL ())
import           Prelude                            (Bool (False), IO, putStrLn,
                                                     return, seq, show, (++))
import           System.Time                        (getClockTime)


t300 :: AVL (InsertN 300)
t300 = insertN (Proxy::Proxy 300) (Proxy::Proxy 'False)

main :: IO ()
main = do t0 <- getClockTime
          seq t300 (return ())
          t1 <- getClockTime
          putStrLn ("Time: " ++ show (secDiff t0 t1) ++ " seconds")
