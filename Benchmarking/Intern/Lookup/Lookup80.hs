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


t80 :: AVL (InsertN 80 'False 'EmptyTree)
t80 = insertN (Proxy::Proxy 80) (Proxy::Proxy 'False) EmptyAVL

v :: Char
v = lookupAVL (Proxy::Proxy 80) t80

main :: IO ()
main = do seq t80 (return ())
          t0 <- getCurrentTime
          seq v (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
