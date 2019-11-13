{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Benchmarking.FullExtern.Operations (InsertN, insertN)

import           Data.Proxy                         (Proxy (Proxy))
import           Extern.AVL                         (AVL ())
import           Prelude                            (Bool (False), IO, putStrLn,
                                                     return, seq, show, (++))
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           ITree (Tree(EmptyTree))


t40 :: AVL (InsertN 40 'False 'EmptyTree)
t40 = insertN (Proxy::Proxy 40) (Proxy::Proxy 'False)

main :: IO ()
main = do t0 <- getCurrentTime
          seq t40 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
