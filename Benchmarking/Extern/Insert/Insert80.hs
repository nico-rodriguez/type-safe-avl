{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Benchmarking.Extern.Operations (InsertN, insertN)
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Extern.AVL                     (AVL (AVL))
import           Prelude                        (Bool (False), IO, putStrLn,
                                                 return, seq, show, (++))

import           ITree (Tree(EmptyTree), ITree(EmptyITree))


t80 :: AVL (InsertN 80 'False 'EmptyTree)
t80 = insertN (Proxy::Proxy 80) (Proxy::Proxy 'False) (AVL EmptyITree)

main :: IO ()
main = do t0 <- getCurrentTime
          seq t80 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
