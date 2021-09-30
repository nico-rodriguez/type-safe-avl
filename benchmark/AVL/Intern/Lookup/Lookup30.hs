{-# OPTIONS_HADDOCK hide, prune #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}


module AVL.Intern.Lookup.Lookup30 (v30, main) where

import           Data.Time.Clock            (diffUTCTime, getCurrentTime)
import           Data.Proxy                 (Proxy (Proxy))
import           Data.Tree.AVL.Intern       (lookupAVL)
import           Prelude                    (IO, putStrLn, return, seq, show, (++))
import           AVL.Intern.Example.Example30 (t30)


v30 = lookupAVL (Proxy::Proxy 29) t30

main :: IO ()
main = do seq t30 (return ())
          t0 <- getCurrentTime
          seq v30 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t1 t0) ++ " seconds")
