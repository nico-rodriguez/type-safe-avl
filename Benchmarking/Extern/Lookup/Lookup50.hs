{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}

import           Benchmarking.Extern.Operations (InsertN, insertN)
import           Benchmarking.Utils             (secDiff)
import           Data.Proxy                     (Proxy (Proxy))
import           Extern.AVL                     (AVL (AVL), lookupAVL)
import           Prelude                        (Bool (False), Char, IO,
                                                 putStrLn, return, seq, show,
                                                 (++))
import           System.Time                    (getClockTime)
import           ITree (Tree(EmptyTree), ITree(EmptyITree))


t50 :: AVL (InsertN 50 'False 'EmptyTree)
t50 = insertN (Proxy::Proxy 50) (Proxy::Proxy 'False) (AVL EmptyITree)

v :: Char
v = lookupAVL (Proxy::Proxy 50) t50

main :: IO ()
main = do seq t50 (return ())
          t0 <- getClockTime
          seq v (return ())
          t1 <- getClockTime
          putStrLn ("Time: " ++ show (secDiff t0 t1) ++ " seconds")
