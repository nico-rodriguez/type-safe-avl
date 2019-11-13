{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Benchmarking.Intern.Operations (InsertN, insertN)
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Intern.AVL                     (AVL (EmptyAVL))
import           Prelude                        (Bool (False), IO, putStrLn,
                                                 return, seq, show, (++))

import           ITree (Tree(EmptyTree))


t30 :: AVL (InsertN 30 'False 'EmptyTree)
t30 = insertN (Proxy::Proxy 30) (Proxy::Proxy 'False) EmptyAVL

main :: IO ()
main = do t0 <- getCurrentTime
          seq t30 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
