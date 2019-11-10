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


t200 :: AVL (InsertN 200)
t200 = insertN (Proxy::Proxy 200) (Proxy::Proxy 'False)
e :: AVL 'EmptyTree
e = deleteN (Proxy::Proxy 200) t200

main :: IO ()
main = do seq t200 (return ())
          t0 <- getClockTime
          seq e (return ())
          t1 <- getClockTime
          putStrLn ("Time: " ++ show (secDiff t0 t1) ++ " seconds")
