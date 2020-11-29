{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}


module AVL.Unsafe.Lookup.Lookup50 (v50, main) where

import           Data.Time.Clock            (diffUTCTime, getCurrentTime)
import           Data.Tree.AVL.Unsafe       (lookupAVL)
import           Prelude                    (IO, putStrLn, return, seq, show, (++))
import           AVL.Unsafe.Insert.Insert50 (t50)


v50 = lookupAVL 49 t50

main :: IO ()
main = do seq t50 (return ())
          t0 <- getCurrentTime
          seq v50 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
