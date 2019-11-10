{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Benchmarking.Intern.Operations (InsertN, deleteN, insertN)
import           Benchmarking.Utils             (secDiff)
import           Data.Proxy                     (Proxy (Proxy))
import           Intern.AVL                     (AVL ())
import           ITree                          (Tree (EmptyTree))
import           Prelude                        (Bool (False), IO, putStrLn,
                                                 return, seq, show, (++))
import           System.Time                    (getClockTime)


t600 :: AVL (InsertN 600)
t600 = insertN (Proxy::Proxy 600) (Proxy::Proxy 'False)
e :: AVL 'EmptyTree
e = deleteN (Proxy::Proxy 600) t600

main :: IO ()
main = do seq t600 (return ())
          t0 <- getClockTime
          seq e (return ())
          t1 <- getClockTime
          putStrLn ("Time: " ++ show (secDiff t0 t1) ++ " seconds")
