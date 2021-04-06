{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}


module AVL.Extern.Lookup.Lookup20 (v20, main) where

import           Data.Time.Clock            (diffUTCTime, getCurrentTime)
import           Data.Proxy                 (Proxy (Proxy))
import           Data.Tree.AVL.Extern       (lookupAVL)
import           Prelude                    (IO, putStrLn, return, seq, show, (++))
import           AVL.Extern.Example.Example20 (t20)


v20 = lookupAVL (Proxy::Proxy 19) t20

main :: IO ()
main = do seq t20 (return ())
          t0 <- getCurrentTime
          seq v20 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
