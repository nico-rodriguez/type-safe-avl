{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Benchmarking.Intern.Operations (InsertN, deleteN, insertN)
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Intern.AVL                     (AVL (EmptyAVL))
import           ITree                          (Tree (EmptyTree))
import           Prelude                        (Bool (False), IO, putStrLn,
                                                 return, seq, show, (++))



t50 :: AVL (InsertN 50 'False 'EmptyTree)
t50 = insertN (Proxy::Proxy 50) (Proxy::Proxy 'False) EmptyAVL
e :: AVL 'EmptyTree
e = deleteN (Proxy::Proxy 50) t50

main :: IO ()
main = do seq t50 (return ())
          t0 <- getCurrentTime
          seq e (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
