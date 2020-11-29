{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}


module AVL.Unsafe.Lookup.Lookup30 (v30, main) where

import           Data.Time.Clock            (diffUTCTime, getCurrentTime)
import           Data.Tree.AVL.Unsafe       (lookupAVL)
import           Prelude                    (IO, putStrLn, return, seq, show, (++))
import           AVL.Unsafe.Insert.Insert30 (t30)


v30 = lookupAVL 29 t30

main :: IO ()
main = do seq t30 (return ())
          t0 <- getCurrentTime
          seq v30 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
