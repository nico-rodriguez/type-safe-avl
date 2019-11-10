{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Benchmarking.Intern.Operations (InsertN, insertN)
import           Benchmarking.Utils             (secDiff)
import           Data.Proxy                     (Proxy (Proxy))
import           Intern.AVL                     (AVL ())
import           Prelude                        (Bool (False), IO, putStrLn,
                                                 return, seq, show, (++))
import           System.Time                    (getClockTime)


t400 :: AVL (InsertN 400)
t400 = insertN (Proxy::Proxy 400) (Proxy::Proxy 'False)

main :: IO ()
main = do t0 <- getClockTime
          seq t400 (return ())
          t1 <- getClockTime
          putStrLn ("Time: " ++ show (secDiff t0 t1) ++ " seconds")
