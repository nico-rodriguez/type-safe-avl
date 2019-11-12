{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Benchmarking.Intern.Operations (InsertN, insertN)
import           Benchmarking.Utils             (secDiff)
import           Data.Proxy                     (Proxy (Proxy))
import           Intern.AVL                     (AVL (EmptyAVL))
import           Prelude                        (Bool (False), IO, putStrLn,
                                                 return, seq, show, (++))
import           System.Time                    (getClockTime)
import           ITree (Tree(EmptyTree))


t10 :: AVL (InsertN 10 'False 'EmptyTree)
t10 = insertN (Proxy::Proxy 10) (Proxy::Proxy 'False) EmptyAVL

main :: IO ()
main = do t0 <- getClockTime
          seq t10 (return ())
          t1 <- getClockTime
          putStrLn ("Time: " ++ show (secDiff t0 t1) ++ " seconds")
