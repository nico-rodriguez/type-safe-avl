{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Benchmarking.Intern.Operations (InsertN, insertN)
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Intern.AVL                     (AVL (EmptyAVL), lookupAVL)
import           Prelude                        (Bool (False), Char, IO,
                                                 putStrLn, return, seq, show,
                                                 (++))

import           ITree (Tree(EmptyTree))


t60 :: AVL (InsertN 60 'False 'EmptyTree)
t60 = insertN (Proxy::Proxy 60) (Proxy::Proxy 'False) EmptyAVL

v :: Char
v = lookupAVL (Proxy::Proxy 60) t60

main :: IO ()
main = do seq t60 (return ())
          t0 <- getCurrentTime
          seq v (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
