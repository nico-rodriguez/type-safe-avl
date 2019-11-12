{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Benchmarking.Extern.Operations (InsertN, insertN)
import           Benchmarking.Utils             (secDiff)
import           Data.Proxy                     (Proxy (Proxy))
import           Extern.AVL                     (AVL (AVL))
import           Prelude                        (Bool (False), IO, putStrLn,
                                                 return, seq, show, (++))
import           System.Time                    (getClockTime)
import           ITree (Tree(EmptyTree), ITree(EmptyITree))


t20 :: AVL (InsertN 20 'False 'EmptyTree)
t20 = insertN (Proxy::Proxy 20) (Proxy::Proxy 'False) (AVL EmptyITree)

main :: IO ()
main = do t0 <- getClockTime
          seq t20 (return ())
          t1 <- getClockTime
          putStrLn ("Time: " ++ show (secDiff t0 t1) ++ " seconds")
