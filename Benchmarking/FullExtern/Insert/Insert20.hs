{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Benchmarking.FullExtern.Operations (InsertN, insertN)

import           Data.Proxy                         (Proxy (Proxy))
import           Extern.AVL                         (AVL ())
import           Prelude                            (Bool (False), IO, putStrLn,
                                                     return, seq, show, (++))
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           ITree (Tree(EmptyTree))


t20 :: AVL (InsertN 20 'False 'EmptyTree)
t20 = insertN (Proxy::Proxy 20) (Proxy::Proxy 'False)

main :: IO ()
main = do t0 <- getCurrentTime
          seq t20 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
