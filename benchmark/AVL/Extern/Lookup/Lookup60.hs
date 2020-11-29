{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}


module AVL.Extern.Lookup.Lookup60 (v60, main) where

import           Data.Time.Clock            (diffUTCTime, getCurrentTime)
import           Data.Proxy                 (Proxy (Proxy))
import           Data.Tree.AVL.Extern       (lookupAVL)
import           Prelude                    (IO, putStrLn, return, seq, show, (++))
import           AVL.Extern.Insert.Insert60 (t60)


v60 = lookupAVL (Proxy::Proxy 59) t60

main :: IO ()
main = do seq t60 (return ())
          t0 <- getCurrentTime
          seq v60 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
