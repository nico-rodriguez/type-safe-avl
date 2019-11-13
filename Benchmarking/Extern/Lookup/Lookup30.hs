{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Benchmarking.Extern.Operations (InsertN, insertN)
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Extern.AVL                     (AVL (AVL), lookupAVL)
import           Prelude                        (Bool (False), Char, IO,
                                                 putStrLn, return, seq, show,
                                                 (++))

import           ITree (Tree(EmptyTree), ITree(EmptyITree))


t30 :: AVL (InsertN 30 'False 'EmptyTree)
t30 = insertN (Proxy::Proxy 30) (Proxy::Proxy 'False) (AVL EmptyITree)

v :: Char
v = lookupAVL (Proxy::Proxy 30) t30

main :: IO ()
main = do seq t30 (return ())
          t0 <- getCurrentTime
          seq v (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
