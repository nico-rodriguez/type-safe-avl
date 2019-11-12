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
import           ITree (Tree(EmptyTree))


t10 :: AVL (InsertN 10 'False 'EmptyTree)
t10 = insertN (Proxy::Proxy 10) (Proxy::Proxy 'False)

v :: Char
v = lookupAVL (Proxy::Proxy 10) t10

main :: IO ()
main = do seq t10 (return ())
          t0 <- getClockTime
          seq v (return ())
          t1 <- getClockTime
          putStrLn ("Time: " ++ show (secDiff t0 t1) ++ " seconds")
