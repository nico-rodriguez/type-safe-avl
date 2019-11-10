{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Benchmarking.FullExtern.Operations (InsertN, insertN)
import           Benchmarking.Utils                 (secDiff)
import           Data.Proxy                         (Proxy (Proxy))
import           Extern.AVL                         (AVL ())
import           Prelude                            (Bool (False), IO, putStrLn,
                                                     return, seq, show, (++))
import           System.Time                        (getClockTime)


t200 :: AVL (InsertN 200)
t200 = insertN (Proxy::Proxy 200) (Proxy::Proxy 'False)

main :: IO ()
main = do t0 <- getClockTime
          seq t200 (return ())
          t1 <- getClockTime
          putStrLn ("Time: " ++ show (secDiff t0 t1) ++ " seconds")
