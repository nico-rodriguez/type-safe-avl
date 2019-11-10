{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Benchmarking.Extern.Operations (InsertN, insertN)
import           Benchmarking.Utils             (secDiff)
import           Data.Proxy                     (Proxy (Proxy))
import           Extern.AVL                     (AVL ())
import           Prelude                        (Bool (False), IO, putStrLn,
                                                 return, seq, show, (++))
import           System.Time                    (getClockTime)


t600 :: AVL (InsertN 600)
t600 = insertN (Proxy::Proxy 600) (Proxy::Proxy 'False)

main :: IO ()
main = do t0 <- getClockTime
          seq t600 (return ())
          t1 <- getClockTime
          putStrLn ("Time: " ++ show (secDiff t0 t1) ++ " seconds")
