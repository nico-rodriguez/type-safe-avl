{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Benchmarking.FullExtern.Operations (InsertN, deleteN, insertN)

import           Data.Proxy                         (Proxy (Proxy))
import           Extern.AVL                         (AVL ())
import           ITree                              (Tree (EmptyTree))
import           Prelude                            (Bool (False), IO, putStrLn,
                                                     return, seq, show, (++))
import           Data.Time.Clock (diffUTCTime, getCurrentTime)


t40 :: AVL (InsertN 40 'False 'EmptyTree)
t40 = insertN (Proxy::Proxy 40) (Proxy::Proxy 'False)
e :: AVL 'EmptyTree
e = deleteN (Proxy::Proxy 40) t40

main :: IO ()
main = do seq t40 (return ())
          t0 <- getCurrentTime
          seq e (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
