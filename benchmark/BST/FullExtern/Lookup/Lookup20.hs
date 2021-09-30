{-# OPTIONS_HADDOCK hide, prune #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}


module BST.FullExtern.Lookup.Lookup20 (v20, main) where

import           Data.Time.Clock                (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Data.Tree.BST.FullExtern       (BST (BST), lookup)
import           Prelude                        (IO, putStrLn, return, seq, show, (++))
import           BST.FullExtern.Example.Example20 (t20)


v20 = case t20 of
  BST t20' _ -> lookup (Proxy::Proxy 19) t20'

main :: IO ()
main = do seq t20 (return ())
          t0 <- getCurrentTime
          seq v20 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
