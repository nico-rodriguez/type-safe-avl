{-# OPTIONS_HADDOCK hide, prune #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe      #-}


module BST.FullExtern.Lookup.Lookup80 (v80, main) where

import           Data.Time.Clock                (diffUTCTime, getCurrentTime)
import           Data.Proxy                     (Proxy (Proxy))
import           Data.Tree.BST.FullExtern       (BST (BST), lookup)
import           Prelude                        (IO, putStrLn, return, seq, show, (++))
import           BST.FullExtern.Example.Example80 (t80)


v80 = case t80 of
  BST t80' _ -> lookup (Proxy::Proxy 79) t80'

main :: IO ()
main = do seq t80 (return ())
          t0 <- getCurrentTime
          seq v80 (return ())
          t1 <- getCurrentTime
          putStrLn ("Time: " ++ show (diffUTCTime t0 t1) ++ " seconds")
