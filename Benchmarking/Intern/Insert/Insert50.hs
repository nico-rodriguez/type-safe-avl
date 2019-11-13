{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Benchmarking.Intern.Operations (InsertN, insertN)
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Intern.AVL                     (AVL (EmptyAVL))
import           Prelude                        (Bool (False), IO, putStrLn,
                                                 return, seq, show, (++))

import           ITree (Tree(EmptyTree))


t50 :: AVL (InsertN 50 'False 'EmptyTree)
t50 = insertN (Proxy::Proxy 50) (Proxy::Proxy 'False) EmptyAVL

main :: IO ()
main = do t0 <- getCurrentTime
          seq t50 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
