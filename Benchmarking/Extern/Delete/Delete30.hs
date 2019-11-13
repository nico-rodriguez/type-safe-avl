{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Benchmarking.Extern.Operations (InsertN, deleteN, insertN)
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Extern.AVL                     (AVL (AVL))
import           ITree                          (Tree (EmptyTree), ITree (EmptyITree))
import           Prelude                        (Bool (False), IO, putStrLn,
                                                 return, seq, show, (++))



t30 :: AVL (InsertN 30 'False 'EmptyTree)
t30 = insertN (Proxy::Proxy 30) (Proxy::Proxy 'False) (AVL EmptyITree)
e :: AVL 'EmptyTree
e = deleteN (Proxy::Proxy 30) t30

main :: IO ()
main = do seq t30 (return ())
          t0 <- getCurrentTime
          seq e (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
