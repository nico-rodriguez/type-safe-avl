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


t30 :: AVL (InsertN 30 'False 'EmptyTree)
t30 = insertN (Proxy::Proxy 30) (Proxy::Proxy 'False) (AVL EmptyITree)

v :: Char
v = lookupAVL (Proxy::Proxy 30) t30

main :: IO ()
main = do seq t30 (return ())
          t0 <- getClockTime
          seq v (return ())
          t1 <- getClockTime
          putStrLn ("Time: " ++ show (secDiff t0 t1) ++ " seconds")
