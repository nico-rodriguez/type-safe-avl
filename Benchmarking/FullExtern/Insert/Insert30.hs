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


t30 :: AVL (InsertN 30 'False 'EmptyTree)
t30 = insertN (Proxy::Proxy 30) (Proxy::Proxy 'False)

main :: IO ()
main = do t0 <- getCurrentTime
          seq t30 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
