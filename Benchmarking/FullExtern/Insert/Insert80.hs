{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Benchmarking.FullExtern.Operations (InsertN, insertN)
import           Benchmarking.Utils                 (diffUTCTime)
import           Data.Proxy                         (Proxy (Proxy))
import           Extern.AVL                         (AVL ())
import           Prelude                            (Bool (False), IO, putStrLn,
                                                     return, seq, show, (++))
import           System.Time                        (getCurrentTime)
import           ITree (Tree(EmptyTree))


t80 :: AVL (InsertN 80 'False 'EmptyTree)
t80 = insertN (Proxy::Proxy 80) (Proxy::Proxy 'False)

main :: IO ()
main = do t0 <- getCurrentTime
          seq t80 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
