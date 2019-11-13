{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Benchmarking.Intern.Operations (InsertN, insertN)
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Intern.AVL                     (AVL (EmptyAVL))
import           Prelude                        (Bool (False), IO, putStrLn,
                                                 return, seq, show, (++))

import           ITree (Tree(EmptyTree))


t60 :: AVL (InsertN 60 'False 'EmptyTree)
t60 = insertN (Proxy::Proxy 60) (Proxy::Proxy 'False) EmptyAVL

main :: IO ()
main = do t0 <- getCurrentTime
          seq t60 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
