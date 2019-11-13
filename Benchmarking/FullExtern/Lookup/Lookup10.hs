{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Benchmarking.FullExtern.Operations (InsertN, insertN)

import           Data.Proxy                         (Proxy (Proxy))
import           Extern.AVL                         (AVL (), lookupAVL)
import           Prelude                            (Bool (False), Char, IO,
                                                     putStrLn, return, seq,
                                                     show, (++))
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           ITree (Tree(EmptyTree))


t10 :: AVL (InsertN 10 'False 'EmptyTree)
t10 = insertN (Proxy::Proxy 10) (Proxy::Proxy 'False)

v :: Char
v = lookupAVL (Proxy::Proxy 10) t10

main :: IO ()
main = do seq t10 (return ())
          t0 <- getCurrentTime
          seq v (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
